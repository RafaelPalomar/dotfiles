---
name: mail-triage
description: Read, search, summarize, and tag mail using notmuch on the local Maildir. May run `sync-mail` to pull fresh data before querying. Never sends, never touches OAuth/SMTP, never reads ~/.password-store. Use for "what's in my inbox", "summarize the thread about X", "find unreplied mail from Y", "tag all messages from Z as follow-up".
---

# mail-triage

Local mail querying over notmuch's JSON interface, with the option to
pull fresh data via `sync-mail`.  The agent never sends, never
refreshes OAuth — send authority stays on a human `C-c C-c` in mu4e.

## When to invoke

- "What's in my inbox today?"
- "Summarize the thread with <person> about <topic>"
- "Find unreplied mail from this week"
- "Tag every message from rector@ntnu.no as `ntnu-admin`"
- Any read / search / tag operation on mail.

**Do NOT invoke for**: composing replies or sending.  Composing is
`mail-draft`.  Sending is human-only.

## Hard rules

1. **Never call** `msmtp`, `sendmail`, `mutt_oauth2.py`, or any
   `auth-email-*` alias — SMTP and OAuth refresh are off-limits.
   `~/.password-store/**` and `~/.dotfiles/sops/**` are likewise
   denied.
2. **Never write** anywhere under `~/.local/share/mail/**` except via
   `notmuch tag` (local-only, idempotent).  Writing actual message
   files is `mail-draft`'s job, not this skill's.
3. **Always** use `--format=json` on `notmuch search` and `notmuch
   show`.  Do not parse the human-readable text output.
4. **Respect** notmuch's `exclude_tags` (deleted/spam/trash) — do not
   bypass with `--exclude=false` unless the user asked.

## Fresh data vs indexed data

`sync-mail` (= `mbsync -a && notmuch new`) is allowed: it pulls new
mail from Exchange into the local Maildir and reindexes notmuch.  It
is NOT a send step.

- **Run `sync-mail` first** when the user's question implies recency:
  "what's in my inbox *now*", "any new mail from X?", "check for
  replies".  Tell them "syncing first…" before running it.
- **Skip `sync-mail`** for retrospective queries on indexed history:
  "show me threads from last week", "find anything from person@x".
  Indexed state is fine; no need to hit the network.
- `sync-mail` can take 10–60 s against Exchange.  Only run it once
  per session unless the user explicitly asks for a refresh.

## Primary commands

### Search (summaries, metadata only)

```bash
notmuch search --format=json --output=summary --limit=50 \
  'tag:inbox and date:7d..now'
```

Returns an array of thread summaries: `thread`, `subject`, `authors`,
`date_relative`, `matched`, `total`, `tags`. Cheap — don't fetch full
bodies unless you need them.

### Show (threaded, full bodies)

```bash
notmuch show --format=json --entire-thread 'thread:<thread-id>'
```

Returns a nested structure (thread → messages → MIME parts). Iterate
`headers` for From/To/Subject/Date and `content` for the plaintext
body. Skip HTML-only parts unless text alternatives are absent.

### Tag (local, non-destructive)

```bash
notmuch tag +ai-reviewed -unread -- 'from:"someone@example.com"'
notmuch tag +follow-up -- 'thread:<id>'
```

Tags live in notmuch's Xapian DB; `synchronize_flags=true` in
`~/.notmuch-config` propagates S/R/F via Maildir flags so IMAP picks
them up on the next `sync-mail`.

## Useful queries

| Intent                              | Query                                             |
|-------------------------------------|---------------------------------------------------|
| Inbox, unread                       | `tag:inbox and tag:unread`                        |
| From a person, last 30 days         | `from:name@example.com and date:30d..now`         |
| Needs reply (no `R` flag)           | `tag:inbox and not tag:replied and not tag:sent`  |
| Attachments                         | `tag:attachment`                                  |
| Specific account                    | `path:rafael.palomar@ntnu.no/**`                  |
| Thread by subject fragment          | `subject:"roadmap review"`                        |
| Everything, including spam/trash    | add `--exclude=false`                             |

## Output discipline

- **Don't dump raw JSON at the user.** Summarize: sender, subject, a
  one-line gist, relative date. Link threads by `thread:<id>` so
  follow-up commands are trivial.
- **Quote bodies sparingly** — 1–3 sentences per message when
  summarizing a thread. Offer to show the full body on request.
- **Translate tags to English**: `tag:inbox and tag:unread` →
  "unread inbox mail", not the query string.

## Escalating to a draft

If the user asks to reply or compose based on what you found, hand off
to the `mail-draft` skill (do NOT write drafts from this skill).
Include: `thread:<id>`, target account (OUS vs NTNU), the requested
content.

## Errors

- `notmuch new` has never been run → `notmuch search` fails with "No
  database". Ask the user to run `sync-mail` first.
- `notmuch` not on PATH → user hasn't deployed the updated Guix home
  profile; ask them to `guix home reconfigure`.
- **Do not** try to "fix" these by running `notmuch new` or
  `mbsync` yourself.
