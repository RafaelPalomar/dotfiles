---
name: mail-triage
description: Read, search, summarize, and tag mail using `notmuch-agent` against the filtered, allowlist-only agent database. Never reads raw maildir, never runs raw `notmuch`, never syncs, never sends, never touches OAuth. Use for "what's in my inbox", "summarize the thread about X", "find unreplied mail from Y", "tag all messages from Z as follow-up".
---

# mail-triage

Local mail querying over `notmuch-agent`'s JSON interface. The agent
never reads raw mail files, never runs raw `notmuch`, never syncs, and
never sends. Send authority stays on a human `C-c C-c` in mu4e.

## When to invoke

- "What's in my inbox today?"
- "Summarize the thread with <person> about <topic>"
- "Find unreplied mail from this week"
- "Tag every message from rector@ntnu.no as `ntnu-admin`"
- Any read / search / tag operation on mail.

**Do NOT invoke for**: composing replies or sending. Composing is
`mail-draft`. Sending is human-only.

## Hard rules

1. **Only use `notmuch-agent`**. Raw `notmuch`, `mbsync`, `sync-mail`,
   and `sync-mail-agent-index` are denylisted. `notmuch-agent`
   supports `search`, `show`, `count`, `address`, and `tag` only —
   any other subcommand is refused.
2. **Never read** `~/.local/share/mail/**` or
   `~/.local/share/mail-agent/**` with the `Read` tool. Maildir
   file contents are reached only via `notmuch-agent show`.
3. **The Sensitive folder is not indexed** in the agent's DB. You
   cannot see messages there; `--exclude=false` will not change
   that. If the user's question points at Sensitive content, tell
   them and suggest they open mu4e themselves.
4. **Never call** `msmtp`, `sendmail`, `mutt_oauth2.py`, or any
   `auth-email-*` alias. SMTP and OAuth are off-limits.
5. **Never write** anywhere under `~/.local/share/mail/**` or
   `~/.local/share/mail-agent/**` except via `notmuch-agent tag`
   (local-only, idempotent, Xapian-side). Writing message files is
   `mail-draft`'s job.
6. **Always** use `--format=json` on `notmuch-agent search` and
   `notmuch-agent show`. Do not parse human-readable text output.
7. **Respect** notmuch's `exclude_tags` (deleted/spam/trash) — do
   not bypass with `--exclude=false`.

## Freshness

The agent's DB at `~/.local/share/mail-agent/` is refreshed only when
the user runs `sync-mail`. The agent **cannot** refresh it.

- **Retrospective queries** (indexed history): proceed directly.
- **Recency-sensitive queries** ("what's in my inbox *now*", "any
  new mail from X?"): tell the user "your agent index may be stale;
  run `sync-mail` to refresh", and proceed against the current
  index without pretending otherwise.

Never attempt to run `sync-mail`, `mbsync`, `notmuch new`, or
`sync-mail-agent-index` — they are denylisted and the denial is
intentional.

## What the agent DB contains

Allowlisted per-account folders only (symlinked from the real
maildir, so contents are identical — only the set of folders is
restricted). The current allowlist lives in
`~/.local/bin/sync-mail-agent-index` and covers:

- `INBOX` — incoming
- `Archive` — filed
- `Sent`, `Sent Items` — sent history
- `Drafts`, `Kladd` — in-progress drafts

**Not in the agent DB** (and therefore invisible to you): `Sensitive`,
`Calendar`, `Contacts`, `Notes`, `Tasks`, `Journal`,
`Conversation History`, `Junk`, `Junk Email`, `Outbox`, `Spambox`,
`Trash`, `Deleted Items`, `Unwanted`, and any future folder the user
hasn't explicitly added to the allowlist.

## Primary commands

### Search (summaries, metadata only)

```bash
notmuch-agent search --format=json --output=summary --limit=50 \
  'tag:inbox and date:7d..now'
```

Returns an array of thread summaries: `thread`, `subject`, `authors`,
`date_relative`, `matched`, `total`, `tags`. Cheap — don't fetch full
bodies unless you need them.

### Show (threaded, full bodies)

```bash
notmuch-agent show --format=json --entire-thread 'thread:<thread-id>'
```

Returns a nested structure (thread → messages → MIME parts). Iterate
`headers` for From/To/Subject/Date and `content` for the plaintext
body. Skip HTML-only parts unless text alternatives are absent.

### Tag (local, non-destructive, scratch-only)

```bash
notmuch-agent tag +ai-reviewed -unread -- 'from:"someone@example.com"'
notmuch-agent tag +follow-up -- 'thread:<id>'
```

Important: agent-side tags live **only** in the agent's Xapian DB,
not in the user's main notmuch DB or mu4e. `synchronize_flags=false`
is set on the agent config so no Maildir flag renames happen (those
files are symlinked from the real maildir — we must not touch them).
If the user wants tags to reach their main DB, they apply them in
mu4e / `notmuch` themselves.

## Useful queries

| Intent                              | Query                                             |
|-------------------------------------|---------------------------------------------------|
| Inbox, unread                       | `tag:inbox and tag:unread`                        |
| From a person, last 30 days         | `from:name@example.com and date:30d..now`         |
| Needs reply (no `R` flag)           | `tag:inbox and not tag:replied and not tag:sent`  |
| Attachments                         | `tag:attachment`                                  |
| Specific account                    | `path:rafael.palomar@ntnu.no/**`                  |
| Thread by subject fragment          | `subject:"roadmap review"`                        |

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

- `notmuch-agent search` returns no results for queries you expect
  to match Sensitive content → working as designed; tell the user.
- `notmuch-agent: subcommand '<foo>' not permitted` → you tried a
  subcommand other than search/show/count/address/tag; use one of
  those instead.
- Agent DB empty or very stale → user has not run `sync-mail` yet,
  or `sync-mail-agent-index` failed for them. Ask the user to run
  `sync-mail`; do NOT try to populate it yourself.
- `notmuch` not on PATH / `notmuch-agent` not on PATH → user hasn't
  deployed the updated Guix home profile; ask them to
  `guix home reconfigure`.
