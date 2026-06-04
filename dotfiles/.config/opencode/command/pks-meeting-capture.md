---
description: "Capture a meeting invite into the user's PKS as a denote meeting note in ~/pks/fleeting/. Wraps `pks-meeting-from-mid` for initial capture and `pks-meeting-sync` for reconciliation against later REQUEST / CANCEL messages from the organizer. ALWAYS asks the user to confirm before writing. Use for 'capture this meeting', 'add that invite to my PKS', 'log the seminar invite'."
---
# pks-meeting-capture

Two-helper skill for keeping calendar invites synchronized with PKS:

- **`pks-meeting-from-mid <mid>`** — first capture. Reads the event
  via `mail-calendar show`, writes a denote meeting note in
  `~/pks/fleeting/` with attendees / agenda / source-link sections,
  and stores the iCal UID + SEQUENCE as `#+ICAL_UID:` /
  `#+ICAL_SEQUENCE:` file-level keywords.
- **`pks-meeting-sync`** — reconciliation. Walks fleeting for notes
  with `#+ICAL_UID:` and queries `mail-calendar show <uid>` for each.
  On SEQUENCE bump, appends a `* Updates` log line and bumps the
  stored sequence. On METHOD:CANCEL, appends a `[CANCELLED]` log
  line.

One note per series: recurring meetings collapse to a single denote
note (UID-keyed); subsequent reschedules become `* Updates` entries on
the same note rather than new notes.

## When to invoke

- "Capture this meeting in my PKS"
- "Add the seminar invite to fleeting"
- "Did anything get rescheduled?" → `pks-meeting-sync --dry-run`
- "Reconcile my meeting notes with the latest invites" → `pks-meeting-sync`

**Do NOT invoke for**:

- Tasks that aren't meetings — use `pks-create` for free-form fleeting
  notes.
- Capturing into a project. Meetings always start in fleeting; promote
  to a project's `* Log` later via the user's `ma` capture template
  if action items emerge.

## Capture flow

1. Find the invite. If the user gave a Message-ID or UID, skip ahead.
   Otherwise use `mail-calendar list` / `mail-calendar week` to locate
   the event and grab its `msg_id` or `uid`.
2. **Show the plan** to the user. Run a `--dry-run` first:

   ```
   pks-meeting-from-mid '<message-id-or-uid>' --dry-run
   ```

   Print the resulting Silo / Title / Keywords / UID / DTSTART /
   first ~10 lines of the body.

3. **Ask for explicit confirmation**: "Create this meeting note in
   `~/pks/fleeting/`? (y/n)". Do not proceed on ambiguous responses.

4. On `y`, run without `--dry-run`:

   ```
   pks-meeting-from-mid '<message-id-or-uid>'
   ```

5. Print the resulting denote path. Suggest the user open it in Emacs
   to fill `* Notes` during the meeting.

## Sync flow

For a periodic check ("anything change with my meetings?"):

1. `pks-meeting-sync --dry-run --verbose` — print the diff.
2. If updates look right, ask the user: "Apply these updates to
   `~/pks/fleeting/`?".
3. On `y`, run `pks-meeting-sync`.

The sync is append-only: it never rewrites existing content, only adds
lines under `* Updates`. The one in-place edit is bumping the
`#+ICAL_SEQUENCE:` line so the next sync diff is computed against the
new baseline.

## Hard rules

1. **Always confirm** before `pks-meeting-from-mid` writes (PKS
   create-note rule). The skill defaults to `--dry-run` first.
2. **Default silo is `~/pks/fleeting/`.** Meeting notes never land
   directly in `~/pks/projects/`. Promote later if needed.
3. **One note per UID.** `pks-meeting-from-mid` refuses to duplicate
   an existing UID; if the user wants to update, run `pks-meeting-sync`
   instead.
4. **CANCEL does not delete the note.** It appends a `[CANCELLED]`
   line under `* Updates`. The user decides during daily review whether
   to archive, promote, or delete.
5. **Read authority is `mail-calendar` only.** Do not call
   `notmuch-agent show 'mimetype:text/calendar'` directly — the iCal
   parsing, UID dedup, and CANCEL handling live in `mail-calendar`.

## Body shape

`pks-meeting-from-mid` writes:

```org
#+ICAL_UID: <uid>
#+ICAL_SEQUENCE: 0
#+ICAL_DTSTART: 2026-05-12T10:00:00+02:00
#+ICAL_DTEND:   2026-05-12T11:00:00+02:00
#+ICAL_RRULE:   FREQ=WEEKLY;BYDAY=TU
#+ICAL_LOCATION: https://teams.microsoft.com/...

* Attendees
- Organizer: <name> <email>
- <attendee 1>
- <attendee 2>

* Agenda
<DESCRIPTION from the invite>

* Notes

* Action items
  - [ ]

* Source
- Invite: [[notmuch:id:<msg-id>][<summary> · <dtstart>]]
- Location: <url-or-string>
```

Plus the standard denote front matter (`#+title:`, `#+date:`,
`#+filetags: :meeting:agenda:<context>:`, `#+identifier:`) prepended
by `denotecli create`.

## Companion skills

- `mail-calendar` — find the Message-ID or UID of an invite.
- `pks-create` — free-form fleeting note (non-meeting).
- `pks-project-log` — for action items that belong to an ongoing
  project.
