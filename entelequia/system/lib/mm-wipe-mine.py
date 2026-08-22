#!/usr/bin/env python3
"""Slash-command handler for /wipe-mine: delete the caller's own posts.

WHY THIS EXISTS AT ALL
Mattermost has no bulk delete in its UI, and `!wipe' on Ms. Poppins or Mr.
Banks can only remove each bot's OWN side of a conversation.  Removing the
family's own messages needs `delete_others_posts', which Mattermost grants to
system admins only -- and `mmctl permissions add' is Enterprise, so there is
no narrow permission to hand a bot.  The only key that fits opens every door
on the server, so it does not live in an agent's sandbox.  It lives here,
behind a slash command a person has to type.

WHY IT RUNS IN THE MATTERMOST NETNS
A service bound to the host's 127.0.0.1 is NOT reachable from the Mattermost
container -- verified, not assumed: host->container works because :8065 is a
published port, but container->host loopback does not.  Binding on the host's
tailnet address would work and would also expose this to every device on the
tailnet, where the slash token would be the only thing standing between a
forged payload and someone else's messages.  Sharing the sidecar's netns
instead means this port exists only for Mattermost: not on the host, not on
the tailnet, not on the LAN.

THE SAFETY PROPERTY
It accepts NO target arguments.  Both the user and the channel come from
Mattermost's signed slash-command payload, so `/wipe-mine' can only ever
delete the posts of whoever typed it, in the channel they typed it in.  There
is no parameter to point it at another person or another room.

WHAT IT DELIBERATELY WILL NOT DO
Soft delete only.  `--permanent' erases attachments from the database and the
filestore, and Mattermost's own tooling asks for a database backup first --
that is a decision to make at a shell with a backup to hand, not by typing a
word into a chat box.  scripts/mm-wipe-mine.sh is where that lives.
"""

import hmac
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

MM_URL = os.environ.get("MM_URL", "http://127.0.0.1:8065")
ADMIN_USER = os.environ.get("MM_ADMIN_USER", "admin")
ADMIN_PW_FILE = os.environ.get("MM_ADMIN_PW_FILE",
                               "/run/secrets/mattermost/admin_password")
SLASH_TOKEN_FILE = os.environ.get(
    "WIPE_TOKEN_FILE", "/var/lib/mattermost-provision/wipe-mine.token")
BIND = os.environ.get("WIPE_BIND", "127.0.0.1")
PORT = int(os.environ.get("WIPE_PORT", "8099"))
#: How far back to look.  A ceiling on purpose: a request that walks a year of
#: history times out against Mattermost's own slash-command deadline, and a
#: wipe that half-finishes is worse than one that says how far it got.
MAX_POSTS = int(os.environ.get("WIPE_MAX_POSTS", "1000"))

API = MM_URL.rstrip("/") + "/api/v4"


def log(*a):
    print("wipe-mine:", *a, file=sys.stderr, flush=True)


def _read(path):
    try:
        with open(path) as fh:
            return fh.read().strip()
    except OSError as e:
        log("cannot read", path, e)
        return ""


def _request(method, path, token=None, body=None, want_header=None):
    """One Mattermost API call.  Returns (parsed-json, header) or (None, None)."""
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(API + path, data=data, method=method)
    req.add_header("Content-Type", "application/json")
    if token:
        req.add_header("Authorization", "Bearer " + token)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            raw = r.read()
            hdr = r.headers.get(want_header) if want_header else None
            return (json.loads(raw) if raw else {}), hdr
    except urllib.error.HTTPError as e:
        log(method, path, "->", e.code, e.reason)
    except (urllib.error.URLError, ValueError, OSError) as e:
        log(method, path, "failed:", e)
    return None, None


class Session:
    """An admin session token, re-fetched when Mattermost stops accepting it.

    Deliberately not a long-lived personal access token: nothing is written to
    disk, so there is no admin credential here to leak beyond the password file
    the provisioner already mounts for its own bootstrap.
    """

    def __init__(self):
        self.token = None

    def login(self):
        pw = _read(ADMIN_PW_FILE)
        if not pw:
            return None
        _, tok = _request("POST", "/users/login",
                          body={"login_id": ADMIN_USER, "password": pw},
                          want_header="Token")
        self.token = tok
        log("admin session", "acquired" if tok else "FAILED")
        return tok

    def get(self):
        return self.token or self.login()

    def retry(self):
        self.token = None
        return self.login()


SESSION = Session()


def channel_posts(channel_id):
    for attempt in (1, 2):
        tok = SESSION.get()
        if not tok:
            return None
        j, _ = _request("GET", "/channels/%s/posts?per_page=%d"
                        % (urllib.parse.quote(channel_id), MAX_POSTS), token=tok)
        if j is not None:
            posts = j.get("posts") or {}
            order = j.get("order") or list(posts)
            return [posts[i] for i in order if i in posts]
        if attempt == 1:
            SESSION.retry()
    return None


def wipe(user_id, channel_id):
    posts = channel_posts(channel_id)
    if posts is None:
        return None
    tok = SESSION.get()
    mine = failed = 0
    for p in posts:
        # Skip what is already gone, what is not theirs, and system join/leave
        # messages -- those are not the caller's to remove and deleting them
        # leaves odd gaps in the channel.
        if p.get("delete_at") or p.get("user_id") != user_id or p.get("type"):
            continue
        j, _ = _request("DELETE", "/posts/" + p["id"], token=tok)
        if j is None:
            failed += 1
        else:
            mine += 1
    log("removed %d, failed %d, for user %s in %s"
        % (mine, failed, user_id, channel_id))
    return {"mine": mine, "failed": failed}


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _reply(self, text):
        # `ephemeral' so the answer is visible only to whoever ran it: the
        # channel does not need a running commentary of the tidying.
        body = json.dumps({"response_type": "ephemeral", "text": text}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        try:
            n = int(self.headers.get("Content-Length") or 0)
        except ValueError:
            n = 0
        form = urllib.parse.parse_qs((self.rfile.read(n) if n else b"").decode(
            "utf-8", "replace"))

        def field(k):
            return (form.get(k) or [""])[0]

        expected = _read(SLASH_TOKEN_FILE)
        got = field("token")
        # compare_digest, not ==, so a wrong token cannot be narrowed down by
        # timing.  An empty expected token means the provisioner has not run
        # yet; refuse rather than accept everything.
        if not expected or not hmac.compare_digest(expected, got):
            log("rejected a request with a bad or missing token")
            self._reply("That command isn't configured correctly — the token "
                        "doesn't match. Nothing was deleted.")
            return

        user_id = field("user_id")
        channel_id = field("channel_id")
        if not user_id or not channel_id:
            self._reply("Mattermost didn't tell me who or where. "
                        "Nothing was deleted.")
            return

        r = wipe(user_id, channel_id)
        if r is None:
            self._reply("I couldn't read this conversation back, so I removed "
                        "nothing.")
            return
        if not r["mine"] and not r["failed"]:
            self._reply("You have no messages here for me to remove.")
            return
        msg = ("Removed %d of your messages from this conversation."
               % r["mine"])
        if r["failed"]:
            msg += " %d wouldn't delete." % r["failed"]
        msg += ("\n\nAttachments stop being part of the conversation but are "
                "not erased from the server — that needs "
                "`scripts/mm-wipe-mine.sh --permanent`, at a shell, with a "
                "database backup to hand.")
        self._reply(msg)

    def do_GET(self):
        # For a health probe.  Says nothing about the instance.
        self.send_response(200)
        self.send_header("Content-Length", "2")
        self.end_headers()
        self.wfile.write(b"ok")

    def log_message(self, *a):
        pass  # the useful lines go through log() instead


def main():
    if not _read(ADMIN_PW_FILE):
        log("FATAL: no admin password at", ADMIN_PW_FILE)
        return 1
    SESSION.login()
    if not _read(SLASH_TOKEN_FILE):
        log("WARNING: no slash token at", SLASH_TOKEN_FILE,
            "— every request will be refused until the provisioner writes it")
    log("listening on %s:%d" % (BIND, PORT))
    ThreadingHTTPServer((BIND, PORT), Handler).serve_forever()
    return 0


if __name__ == "__main__":
    sys.exit(main())
