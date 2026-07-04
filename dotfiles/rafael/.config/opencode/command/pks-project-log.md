---
description: "Append a dated entry to ~/pks/projects/<project>.org's '* Log' section. Two modes — session-end (low bar, one-line 'what changed, what's next', offered after substantive sessions) and decision (high bar, multi-line context for architectural calls, rejected approaches, patterns). Always asks the user to confirm before append."
---
# pks-project-log

Append to a registered project note's Log section. Pick a mode based
on what just happened.

## Mode A — session-end (default, low bar)

Offer at the end of any substantive session in a registered project,
even routine implementation work. The Log becomes a chronological
narrative readable in the future; missing entries are the failure
mode, not over-eager ones.

Format (one line):

```
- <ISO-date> :: <what changed in this session, plus what's next>
```

Examples that DO deserve a session-end entry:

- `- 2026-05-07 :: Implemented C-c n D + pks-daily-review fallback. Next: SessionStart hook reading the note.`
- `- 2026-05-07 :: Migrated container services to shepherd-root. Podman missing on PATH after, fixed in follow-up.`

What still doesn't deserve an entry:

- Read-only sessions ("looked at code, didn't change anything").
- Single-line typo fixes / linter pass.
- Sessions where the user said "no log".

When the user accepts a session-end entry, do not also propose a
decision entry — Mode A subsumes it for routine work.

## Mode B — decision (high bar, on demand)

Use Mode B (instead of or alongside A) when the session involves:

- Architectural decision with explicit justification.
- Rejected approach — record why, prevents re-deliberation.
- Pattern crystallising across multiple touchpoints.
- Discovery that changes understanding of the project.
- Non-obvious constraint or invariant revealed.

Format (summary + 1–3 sentences of context, optional cross-link):

```
- <ISO-date> :: <summary>
  <context>. [[denote:ID][optional linked note]]
```

## Flow (both modes)

1. Draft the entry (Mode A: one line. Mode B: summary + context).
2. Show the proposed text and ask: *"Append to <project>'s Log? [y/N/edit]"*.
   Default rejection is "no" — but the question is binary-with-edit,
   not open-ended.
3. Find the project note:

   ```
   denotecli search "$(basename $PWD)" \
     --dirs ~/pks/projects --tags project --title-only --max 1
   ```

4. Use the Edit tool to insert after the `* Log` heading:

   ```
   old: * Log\n
   new: * Log\n- <date> :: <summary>\n  [context if Mode B]\n
   ```

5. Save. Briefly confirm. Optionally suggest an auto-memory pointer
   if the content is also useful cross-session.

## Safety

- Never rename the project note or change its keywords from here.
- Never rewrite existing Log entries — append-only.
- If Edit finds multiple `* Log` headings (malformed note), abort
  and report.
