#!/usr/bin/env python3
"""Minimal NextCloud Deck <-> MCP StreamableHTTP shim.

Pure Python stdlib (http.server + urllib + json) — no third-party deps, so the
Guix package needs only `python` as an input.  Bridges the Hermes household
agent (Mary Poppins) to the NextCloud Deck REST API:

  agent --(MCP url transport, loopback in the ts-mattermost netns)--> shim
  shim  --(HTTPS Basic-auth over the tailnet)--> NextCloud Deck on lovelace

Two auth boundaries:
  1. INBOUND  (caller -> shim): every /mcp request must carry
     `Authorization: Bearer <SHIM_BEARER>`.  The kids' tutor tier shares the
     netns and can TCP-connect to the loopback port, but its Hermes config holds
     no Bearer for this server, so it gets 401 before any tool runs.
  2. OUTBOUND (shim -> NextCloud): HTTP Basic with NEXTCLOUD_USER + an
     app-password (NEXTCLOUD_PASS) + the mandatory OCS-APIRequest header.  The
     credential lives ONLY here — it is never sent to the LLM.

Config via env: NEXTCLOUD_URL, NEXTCLOUD_USER, NEXTCLOUD_PASS (app-password),
SHIM_BEARER, optional SHIM_HOST (default 127.0.0.1), SHIM_PORT (default 8765).
"""
import json, os, hmac, base64, urllib.request, urllib.error
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

NC_URL  = os.environ["NEXTCLOUD_URL"].rstrip("/")
NC_USER = os.environ["NEXTCLOUD_USER"]
NC_PASS = os.environ["NEXTCLOUD_PASS"]          # NextCloud app-password
BEARER  = os.environ["SHIM_BEARER"]
API     = f"{NC_URL}/index.php/apps/deck/api/v1.0"
PROTO   = "2025-03-26"

_basic = base64.b64encode(f"{NC_USER}:{NC_PASS}".encode()).decode()

def deck(method, path, body=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(f"{API}{path}", data=data, method=method, headers={
        "Authorization": f"Basic {_basic}",
        "OCS-APIRequest": "true",
        "Content-Type": "application/json",
        "Accept": "application/json",
    })
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            raw = r.read()
            return json.loads(raw) if raw else None
    except urllib.error.HTTPError as e:
        raise RuntimeError(f"Deck {method} {path} -> {e.code}: {e.read()[:300]!r}")

# ── tool implementations ──────────────────────────────────────────────────────
def t_list_boards(_):
    return [{"id": b["id"], "title": b["title"]} for b in deck("GET", "/boards")]

def t_list_stacks(a):
    s = deck("GET", f"/boards/{int(a['board'])}/stacks")
    return [{"id": x["id"], "title": x["title"], "order": x.get("order")} for x in s]

def t_create_card(a):
    body = {"title": a["title"], "type": "plain", "order": int(a.get("order", 999))}
    if a.get("description") is not None:
        body["description"] = a["description"]
    c = deck("POST", f"/boards/{int(a['board'])}/stacks/{int(a['stack'])}/cards", body)
    return {"id": c["id"], "title": c["title"], "stackId": c["stackId"]}

def t_list_cards(a):
    st = deck("GET", f"/boards/{int(a['board'])}/stacks/{int(a['stack'])}")  # cards nested
    return [{"id": c["id"], "title": c["title"], "description": c.get("description"),
             "order": c.get("order")} for c in (st.get("cards") or [])]

TOOLS = {
    "list_boards": (t_list_boards, "List all Deck boards visible to the bot user.",
        {"type": "object", "properties": {}, "additionalProperties": False}),
    "list_stacks": (t_list_stacks, "List the stacks (columns) of a board.",
        {"type": "object", "properties": {"board": {"type": "integer",
         "description": "Board id"}}, "required": ["board"], "additionalProperties": False}),
    "create_card": (t_create_card, "Create a card in a stack of a board.",
        {"type": "object", "properties": {
            "board": {"type": "integer"}, "stack": {"type": "integer"},
            "title": {"type": "string", "maxLength": 255},
            "description": {"type": "string"},
            "order": {"type": "integer", "default": 999}},
         "required": ["board", "stack", "title"], "additionalProperties": False}),
    "list_cards": (t_list_cards, "List the cards in a stack of a board.",
        {"type": "object", "properties": {
            "board": {"type": "integer"}, "stack": {"type": "integer"}},
         "required": ["board", "stack"], "additionalProperties": False}),
}

def tools_list():
    return [{"name": n, "description": d, "inputSchema": s} for n, (f, d, s) in TOOLS.items()]

# ── JSON-RPC dispatch ──────────────────────────────────────────────────────────
def handle_rpc(msg):
    mid, method, params = msg.get("id"), msg.get("method"), msg.get("params") or {}
    def ok(result): return {"jsonrpc": "2.0", "id": mid, "result": result}
    def err(code, m): return {"jsonrpc": "2.0", "id": mid, "error": {"code": code, "message": m}}
    if method == "initialize":
        return ok({"protocolVersion": PROTO, "capabilities": {"tools": {}},
                   "serverInfo": {"name": "deck-shim", "version": "0.1.0"}})
    if method in ("notifications/initialized", "notifications/cancelled"):
        return None                                   # notification -> 202, no body
    if method == "tools/list":
        return ok({"tools": tools_list()})
    if method == "tools/call":
        name = params.get("name"); args = params.get("arguments") or {}
        if name not in TOOLS:
            return err(-32602, f"unknown tool {name}")
        try:
            out = TOOLS[name][0](args)
            return ok({"content": [{"type": "text", "text": json.dumps(out)}]})
        except Exception as e:                         # surface as tool error, not transport error
            return ok({"content": [{"type": "text", "text": str(e)}], "isError": True})
    return err(-32601, f"method not found: {method}")

class H(BaseHTTPRequestHandler):
    def _auth_ok(self):
        h = self.headers.get("Authorization", "")
        want = f"Bearer {BEARER}"
        return len(h) == len(want) and hmac.compare_digest(h, want)
    def _origin_ok(self):                              # DNS-rebind guard (MCP spec)
        o = self.headers.get("Origin")
        return o is None or o.startswith(("http://127.0.0.1", "http://localhost"))
    def _send(self, code, obj=None):
        body = b"" if obj is None else json.dumps(obj).encode()
        self.send_response(code)
        if obj is not None: self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers(); self.wfile.write(body)
    def do_GET(self):  self._send(405)                 # no server-initiated SSE
    def do_DELETE(self): self._send(405)
    def do_POST(self):
        if self.path.rstrip("/") != "/mcp":      return self._send(404)
        if not self._origin_ok():                return self._send(403)
        if not self._auth_ok():                  return self._send(401)   # tutor lands here
        n = int(self.headers.get("Content-Length", 0))
        try:    msg = json.loads(self.rfile.read(n) or b"{}")
        except Exception:
            return self._send(400, {"jsonrpc": "2.0", "id": None,
                                    "error": {"code": -32700, "message": "parse error"}})
        if isinstance(msg, list):                      # JSON-RPC batch
            outs = [r for r in (handle_rpc(m) for m in msg) if r is not None]
            return self._send(202) if not outs else self._send(200, outs)
        resp = handle_rpc(msg)
        return self._send(202) if resp is None else self._send(200, resp)
    def log_message(self, *a): pass                    # quiet

if __name__ == "__main__":
    host = os.environ.get("SHIM_HOST", "127.0.0.1")
    port = int(os.environ.get("SHIM_PORT", "8765"))
    ThreadingHTTPServer((host, port), H).serve_forever()
