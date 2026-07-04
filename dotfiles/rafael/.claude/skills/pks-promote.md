---
name: pks-promote
description: Promote a fleeting note to a permanent note. Moves from ~/pks/fleeting to ~/pks/permanent, replaces the _fleeting keyword with _perm + topical keywords, preserves the denote ID (keeps backlinks alive). Asks user to confirm new title + keywords before acting.
---

# pks-promote

Move a note from `fleeting/` to `permanent/` while preserving its ID.

## Preconditions

- Target note lives in `~/pks/fleeting/`.
- User has confirmed the title (edit if needed) and the permanent
  keywords.

## Flow

1. `denotecli read <ID> --dirs ~/pks/fleeting` — fetch current title,
   keywords, content preview.
2. Propose:
   - Target silo: `~/pks/permanent`
   - New keywords: drop `fleeting`, add `perm` + 1-2 topical keywords
     from the closed set.
   - Title: suggest a rewritten title that asserts a single claim
     (atomic-note rule).
3. **Ask the user to confirm.** Required.
4. Execute:

   ```
   denotecli rename <ID> \
     --dir ~/pks/permanent \
     --tags perm,<topical1>,<topical2> \
     --keep-id
   ```

   (If denotecli's rename doesn't support `--dir` on the installed
   version, fall back to `mv` + `denotecli rename-tag`; verify first
   with `denotecli --help`.)

5. After success, run `denotecli graph <ID> --dirs ~/pks` and verify
   inbound links still resolve.

## Hard rules

- **Never regenerate the ID.** Use `--keep-id`.
- **Never bulk-promote.** One note at a time.
- **If rename fails**: report and stop. Do not retry with a destructive
  fallback.
- Permanent notes should assert a single claim in the title. If the
  fleeting title is vague ("interesting thing"), push the user to
  sharpen it before promoting.
