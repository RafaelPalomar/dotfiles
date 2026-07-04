---
name: mail-to-fleeting
description: Capture a mu4e/notmuch mail thread as a durable denote note in ~/pks/fleeting/. Use when the user asks to "save this thread", "capture this email to PKS", or "make a note from this conversation". Wraps notmuch-agent (read) + denotecli create. The agent path adds an LLM-written summary; the shell fallback `mail-to-fleeting` produces the same structural note without commentary.
---

# mail-to-fleeting

The agent counterpart to the mu4e capture template (`C-c c m e`). Use
when the user wants to durably capture a thread *via Claude*, with a
written summary, rather than triaging it themselves in mu4e.

## When to use

- *"Capture this thread as a fleeting note."*
- *"Save this email about <topic> to PKS."*
- *"Make a note from the conversation with <person>."*

Do NOT use when:

- The user just wants a chat-only summary (don't write a denote note
  unless asked to *capture* or *save*).
- The thread is in the Sensitive folder. The agent's notmuch DB does
  not index Sensitive, so you cannot read it. Tell the user to
  capture from mu4e directly with `C-c c m e`.

## Flow

1. Identify the message-id of the thread root. The user often supplies
   it; otherwise extract via `notmuch-agent search ... --format=json`.

2. Read the thread metadata:

   ```bash
   notmuch-agent show id:<MSG_ID> --format=json
   # or for a thread:
   notmuch-agent show thread:<THREAD_ID> --format=json
   ```

   Capture: Subject, From, Date, To, Cc — these become note properties.

3. Determine the keyword tag:

   - `ous` — sender or recipient at `ous-research.no` / `@ous`.
   - `ntnu` — sender or recipient at `ntnu.no`.
   - Skip the tag if the address pool is purely external.

4. Compose a 3–6 line summary of the thread: who is asking what, what
   decisions were made, what action items remain. **Do not paste full
   message bodies into the note** — the message-id property below is
   the canonical reference; the summary is your contribution.

5. Create the denote note:

   ```bash
   denotecli create \
     --title "<subject>" \
     --tags "agenda,<ous|ntnu>" \
     --dir ~/pks/fleeting/ \
     --content "<body block from step 6>"
   ```

6. Body block format:

   ```
   :PROPERTIES:
   :FROM:    <sender>
   :DATE:    <date>
   :MSG-ID:  <msg-id>
   :SUBJECT: <subject>
   :END:

   <2–4 sentence summary of the thread.>

   <If there are action items: a bullet list under "* Action items".>

   <If the thread maps to a registered project, append a
   [[denote:PROJECT_ID][project name]] cross-link.>
   ```

7. Confirm to the user with the path returned by `denotecli create`.
   Do not also paste the file content — they can open the path.

## Shell-script fallback

`~/.local/bin/mail-to-fleeting <message-id> [ous|ntnu]` produces the
same structural note WITHOUT a summary. Use this when the LLM path is
unavailable or when the user only wants the structural skeleton (and
plans to fill in the body later).

## Hard rules

- Read via `notmuch-agent` only; never `notmuch` raw.
- Never include full message bodies in the note — the message-id is
  the durable reference. Keep the summary 3–6 lines.
- Account/email addresses go in properties, not in the body — they're
  PII and the body becomes the part the user re-reads later.
- Don't strip the message-id; it's load-bearing for cross-reference.
- If the thread resolves to a registered project (`pks-project-context`
  knows which), suggest appending an `[[denote:ID]]` link to that
  project — but write the fleeting note first; offer the link as a
  follow-up.

## After capture

If the thread carries an action item that the user accepts:

- Suggest `pks-project-log` (Action Item mode) to add a Log entry on
  the relevant project.
- Or, for waiting-on-someone-else items: same, with the WAITING marker.

Don't do both fleeting-capture AND project-log automatically — pick
one based on what the user asked for. Capture is for *knowledge*; Log
is for *tracking*.
