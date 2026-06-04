---
description: "Archive a fleeting note — move from ~/pks/fleeting to ~/pks/archive/{work,personal}. Use when a note is worth keeping but won't become permanent (light meeting notes, ephemeral observations, low-consequence traces). Drops the _fleeting keyword, preserves the denote ID, asks user to confirm the target domain (work vs personal) before acting. One-way and explicit — not a substitute for delete, not a waiting room for promotion."
---
# pks-fleeting-archive

Move a note from `fleeting/` to `archive/{work,personal}/` while
preserving its ID.  Archive is **cold storage** — the explicit
decision that this note is worth keeping but won't ripen into a
permanent claim.

## When to use

- Light meeting notes with no follow-up actions.
- Ephemeral observations the user wants a trace of.
- Captures that have aged past the stale-fleeting threshold (>14d)
  and the user has chosen "neither promote nor delete."

## When NOT to use

- The note asserts a durable claim → use `pks-promote` instead.
- The note has zero future value → just delete it.
- The note is still actionable / part of in-flight work → leave in
  fleeting until resolved.
- Bulk-archiving stale fleeting notes — single-note operations only.

## Preconditions

- Target note lives in `~/pks/fleeting/`.
- User has confirmed the target domain (`work` vs `personal`).
- User has confirmed the note should not be promoted to permanent.

## Flow

1. `denotecli read <ID> --dirs ~/pks/fleeting` — fetch current title,
   keywords, content preview.
2. Propose:
   - Target silo: `~/pks/archive/work` or `~/pks/archive/personal`
     (infer from existing keywords like `_ous`/`_ntnu`, but ask if
     ambiguous).
   - New keywords: drop `fleeting`; keep topical keywords as-is.  Do
     NOT add an `_archive` keyword — the silo is the marker
     (consistent with the function-based design).
   - Title: unchanged.  Unlike promotion, archiving doesn't demand
     sharpening — the whole point is that this note isn't going to
     be refined.
3. **Ask the user to confirm.** Required.
4. Execute:

   ```
   denotecli rename <ID> \
     --dir ~/pks/archive/<work|personal> \
     --tags <existing-tags-minus-fleeting> \
     --keep-id
   ```

   (If denotecli's rename doesn't support `--dir` on the installed
   version, fall back to `mv` + `denotecli rename-tag`; verify first
   with `denotecli --help`.)

5. After success, run `denotecli graph <ID> --dirs ~/pks` and verify
   inbound links still resolve.

## Hard rules

- **Never regenerate the ID.** Use `--keep-id`.  Archived notes may
  still be referenced from elsewhere in the graph.
- **Never bulk-archive.** One note at a time.  If the user is
  triaging stale fleeting in batch, walk each one individually.
- **Archive is one-way.**  Notes don't move out of archive back to
  fleeting.  If the user later realises an archived note deserves
  promotion, treat that as a new permanent note that links to (not
  replaces) the archived trace.
- **No auto-archive.**  Never archive on a timer or as part of a
  digest sweep without explicit per-note confirmation.
- **If rename fails**: report and stop.  Do not retry with a
  destructive fallback.
