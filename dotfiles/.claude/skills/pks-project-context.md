---
name: pks-project-context
description: At session start, detect whether the current working directory has a registered project note in ~/pks/projects. If registered, load its Status/Log/Architecture/References into session context. If not registered, invoke pks-project-register (which asks the user). Invoke early, silently if registered.
---

# pks-project-context

Bootstraps PKS awareness for the current working directory.

## Detection

```
proj=$(basename "$PWD")
denotecli search "$proj" \
  --dirs ~/pks/projects \
  --tags project \
  --title-only \
  --max 1
```

Parse JSON. Empty array = unregistered. One hit = registered.

## Registered path

1. Fetch the note: `denotecli read <ID> --dirs ~/pks/projects
   --outline` then `--limit 0` for full content if < 500 lines.
2. Extract `* Status`, `* Log` (last 10 entries), `* Architecture /
   patterns`, `* References` sections.
3. Carry these as implicit session context. **Do not re-announce on
   every turn.** A single "(loaded PKS context for <proj>)" at session
   start is enough.
4. When a log-worthy event occurs during work, offer
   `pks-project-log`.

## Unregistered path

1. Skip prompting entirely for transient sessions: `git log`, `ls`,
   single-command queries, simple questions not intended to modify the
   repo.
2. For substantive sessions (implementing, refactoring, planning):
   **ask once**:

   > This project (`<proj>`) isn't in your PKS. Want me to register
   > it? (Worth it for sustained projects; skip for one-offs.)

3. On **yes**: invoke `pks-project-register`.
4. On **no**: remember the decline in session-only state. **Do not
   nag**. Do not persist a "never register" marker anywhere.

## Heuristics for "substantive session"

Invoke registration prompt when you observe any of:
- User asks to implement, refactor, plan, or add features.
- User asks you to modify multiple files.
- User invokes `aider` / another coding tool from CWD.

Skip the prompt when:
- User runs only read-only commands.
- User is in a scratch/experiment dir unlikely to persist.
- User has previously declined in the same session.

## Memory coordination

On successful registration or context load, save a brief auto-memory
entry pointing to the PKS note:

```
project_<proj>.md:
  [Title](ref_to_pks_note.md) — one-line summary
```

Full context lives in the PKS note; memory is just a pointer.
