---
name: mail-calendar
description: Read meeting invites that arrived as text/calendar mail, via the local `mail-calendar` helper. Emits structured JSON (UID, DTSTART, RRULE, attendees, location) parsed from iCalendar payloads in the agent's filtered notmuch DB. Use for "what meetings do I have this week", "who's organising the X meeting", "any invites for next Tuesday". Read-only — never sends, never syncs, never touches the maildir.
---

# mail-calendar

Local-only view of meeting invites. Wraps `mail-calendar` (which calls
`notmuch-agent show --format=json`, parses iCal via python-icalendar,
and dedupes by UID + SEQUENCE). Calendar truth lives in Outlook and
on the user's phone — this helper sees only what arrived as mail.

## When to invoke

- "What meetings do I have this week?"
- "Any invites for Tuesday afternoon?"
- "Who's organising the MESHLab sync?"
- "Did anything get rescheduled today?"
- Before `pks-meeting-capture` — to find the Message-ID or UID of an
  invite the user wants to capture.

**Do NOT invoke for**:

- Sending invites or RSVPing — that stays in Outlook / mu4e.
- "What's actually on my calendar right now" (i.e. items the user
  accepted on their phone that the organizer never re-sent). The
  agent only sees mailed invites.

## Subcommands

```
mail-calendar list  [--since YYYY-MM-DD] [--until YYYY-MM-DD]
                    [--account ous|ntnu] [--method REQUEST|CANCEL|*]
                    [--no-dedup]
mail-calendar show  <message-id-or-uid>
mail-calendar today
mail-calendar week
```

`list` / `today` / `week` emit newline-delimited JSON (one event per
line). `show` emits a single pretty-printed JSON object.

Default behavior collapses each series to its highest-SEQUENCE REQUEST
and drops METHOD:CANCEL series entirely. Pass `--no-dedup` only when
you specifically want to see RSVP / CANCEL traffic.

## Event fields

```jsonc
{
  "uid":        "040000008200E0...",   // iCal UID, stable across edits
  "method":     "REQUEST",             // REQUEST | CANCEL | REPLY (filtered)
  "sequence":   0,
  "summary":    "MESHLab weekly",
  "dtstart":    "2026-05-12T10:00:00+02:00",
  "dtend":      "2026-05-12T11:00:00+02:00",
  "rrule":      "FREQ=WEEKLY;BYDAY=TU",
  "location":   "https://teams.microsoft.com/l/...",
  "description": "...",
  "organizer":  {"name": "...", "email": "..."},
  "attendees":  [{"name": "...", "email": "...", "partstat": "..."}],
  "msg_id":     "<...@outlook.com>",
  "account":    "ous"                  // or "ntnu" / "unknown"
}
```

## Hard rules

1. **Only use `mail-calendar`**. Do not bypass it by calling
   `notmuch-agent show 'mimetype:text/calendar'` directly — the
   parsing, dedup, and CANCEL handling live in the helper.
2. **Read-only.** `mail-calendar` never writes anywhere. Use
   `pks-meeting-capture` to turn an event into a denote note.
3. **Stale-index errors are recoverable.** If `mail-calendar` reports
   "stale index? run `sync-mail`", tell the user — `sync-mail` is a
   human-only step.
4. **The `Calendar` folder is in the allowlist** (since the May 2026
   capability landed). Auto-accepted invites moved by Outlook rules
   from INBOX to Calendar are visible. Other folders (Sensitive, Junk,
   Deleted Items, ...) remain absent from the agent DB.

## Common patterns

**This week's invites**:

```
mail-calendar week | jq -c '{summary, dtstart, organizer: .organizer.name}'
```

**Find a Teams URL for a specific meeting**:

```
mail-calendar list --since 2026-05-08 --until 2026-05-15 \
  | jq -r 'select(.summary|test("MESHLab"; "i")) | .location'
```

**Look up by UID** (e.g. when a previously-captured note's UID is
reported by `pks-meeting-sync`):

```
mail-calendar show '040000008200E0...'
```

## Companion skills

- `pks-meeting-capture` — turn an event into a denote meeting note.
- `mail-triage` — read / search / tag mail itself.
- `mail-draft` — compose replies (never sends).
