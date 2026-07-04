---
name: pks-project-register
description: Register the current working directory as a project in the user's PKS by creating a denote note in ~/pks/projects. ALWAYS asks the user to confirm — project name, title, domain keyword(s). Uses the project template (Status, Next actions, Log, Architecture, References).
---

# pks-project-register

Create a new project note in `~/pks/projects/`.

## Precondition

Called by `pks-project-context` after the user said yes to the
registration prompt, OR explicitly by the user saying "register this
project".

## Flow

1. Derive default project name: `basename $PWD`.
2. Ask for domain keyword(s) — pick from `_code _research _learn
   _ntnu _ous` (multiple allowed). Always include `_project _agenda`.
3. Ask for the title — suggest "PROJECT <name>" but let the user
   customise.
4. Show the plan:

   ```
   Silo:     ~/pks/projects
   Title:    PROJECT <name>
   Keywords: project, agenda, <domain>
   Template: project (Status/Next actions/Log/Architecture/References)
   ```

5. **Confirm.** Wait for explicit y.
6. Execute:

   ```
   denotecli create \
     --title "PROJECT <name>" \
     --tags project,agenda,<domain> \
     --dir ~/pks/projects \
     --content "$(cat <<'EOF'
* Status
Fresh — no work logged yet.

* Next actions
** NEXT

* Log

* Architecture / patterns

* References
EOF
)"
   ```

7. **Drop a `.pks-project-id` marker at the project root.** This is
   what the commit/stop hooks walk up to find; without the marker the
   hooks fall back to basename-guessing which only works when the
   repo's directory name matches the note title.

   ```
   echo "<DENOTE-ID>" > "$CWD/.pks-project-id"
   ```

   Commit the marker to the repo so every machine with this checkout
   sees the same project.  Single line, plays well with any VCS.

8. Print the resulting path.  Offer to "seed the Log with recent
   memory context about this project" if memory already has relevant
   entries (check `/home/rafael/.claude/projects/*/memory/` for files
   whose names mention the project).

## After creation

- Save a short auto-memory pointer in
  `/home/rafael/.claude/projects/.../memory/project_<proj>.md`:

  ```
  ---
  name: project <proj>
  description: Registered in PKS on <date>
  type: project
  ---
  PKS note: ~/pks/projects/<ID>--<slug>__project_agenda_<dom>.org
  See the note's Log for durable decisions.
  ```

- Do not duplicate the PKS note's content in memory.

## Hard rules

- **Never auto-register.** Always ask first.
- **Never overwrite an existing project note** with the same name —
  if `denotecli search` found a match, abort with a message.
- One project = one note. If the user wants to split into
  subprojects, use `_project` + child topic MOCs in `reference/`.
