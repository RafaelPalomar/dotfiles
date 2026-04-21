---
name: pks-project-log
description: Append a dated decision, rejected approach, pattern, or discovery to the ~/pks/projects/<project>.org note's "* Log" section. Always asks user to confirm the summary text before append. Routine task completions, commit messages, and minor fixes are NOT log-worthy — refuse those.
---

# pks-project-log

Append a durable-knowledge entry to a registered project note's Log.

## When to offer

Offer an append when the current work session involves:

- **Decision with justification** — chose X over Y because of Z.
- **Rejected approach** — document why, prevents future re-deliberation.
- **Pattern crystallising** — "every machine goes through
  machine-config record".
- **Discovery that changes understanding** — "turns out the sops
  extension module failed due to a channel version mismatch".
- **Non-obvious constraint or invariant** — "denotecli only indexes
  files matching the denote filename regex; PDFs in library/ are
  silently skipped".

## When NOT to offer

Skip the log for:

- Routine task completions ("added imports").
- Commit messages (git carries those).
- Linter fixes, minor refactors.
- In-flight blockers (use auto-memory instead).

## Flow

1. Propose a one-line summary + 1-3 sentences of context.
2. Optionally include one `[[denote:ID]]` link to a relevant
   permanent or literature note.
3. Format:

   ```
   - <ISO-date> :: <summary>
     <context>. [[denote:ID][optional linked note]]
   ```

4. **Ask the user to confirm** the text. Do not proceed on vague
   approval.
5. Find the project note:

   ```
   denotecli search "$(basename $PWD)" \
     --dirs ~/pks/projects --tags project --title-only --max 1
   ```

6. Read current content, append to the `* Log` section, write back.

   Since denotecli lacks a direct append operation, perform:
   ```bash
   path=$(jq -r '.[0].path' <<<"$SEARCH_JSON")
   # Locate "* Log" and insert the new entry immediately after its heading.
   # Use a file-editor tool (Edit) for the actual write; do not
   # hand-roll sed/awk that could corrupt org structure.
   ```

   Prefer using the Edit tool with the exact heading anchor:

   ```
   old: * Log\n
   new: * Log\n- <date> :: <summary>\n  <context>\n
   ```

7. After append, save and confirm to user. Optionally suggest a
   brief auto-memory pointer if the content is also useful
   cross-session.

## Safety

- Never rename the project note or change its keywords from this
  skill.
- Never rewrite existing Log entries.
- If Edit finds multiple `* Log` headings (shouldn't happen but is
  possible in malformed notes), abort and report.
