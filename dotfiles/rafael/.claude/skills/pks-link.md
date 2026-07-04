---
name: pks-link
description: Help the user insert a denote link between two notes. Identifies the target by search, produces the [[denote:ID][title]] snippet, and optionally shows where dblocks (denote-backlinks, denote-links, denote-missing-links) could be added. Does NOT auto-edit files — emits the snippet for the user to paste or for an editor agent to apply.
---

# pks-link

Generate denote-style links between notes.

## Flow

1. User names a source note (by title/path) and a target concept.
2. Use `pks-search` to locate candidate target(s).
3. Confirm the chosen target.
4. Emit the link snippet:

   ```
   [[denote:YYYYMMDDTHHMMSSID][title]]
   ```

5. Offer to insert a dynamic block in the source note:

   - `#+BEGIN: denote-backlinks` — lists notes linking here.
   - `#+BEGIN: denote-links :regexp "_keyword"` — lists notes matching
     filter.
   - `#+BEGIN: denote-missing-links :keyword "foo"` — lists notes in
     the silo that share the keyword but aren't yet linked.

## Safety

- Never modify a note's body from this skill. Only produce snippets.
- If the user asks to apply the link, delegate to a file-editing flow
  (Edit tool or Emacs) with the explicit diff.
- Verify target ID exists via `denotecli read <ID>` before emitting
  the link.
