---
name: pks-create
description: Create a new denote note in a specific silo of ~/pks. ALWAYS asks user to confirm title, silo, and keywords before writing. Default silo is fleeting/. Use for user-directed note creation; use pks-project-register for project notes, pks-promote for fleeting→permanent.
---

# pks-create

Create a denote-formatted note in `~/pks/<silo>/` via denotecli.

## Silo choices

- `~/pks/fleeting` — default for quick captures (keywords include
  `fleeting`).
- `~/pks/permanent` — atomic evergreen (keywords include `perm`).
  Prefer `pks-promote` when converting a fleeting note.
- `~/pks/literature` — source-anchored. Prefer Emacs citar workflow
  when the user has the `.bib` entry; only use this skill if the user
  explicitly asks to create a lit note directly.
- `~/pks/projects` — project notes. Prefer `pks-project-register`
  because it applies the correct template and _agenda keyword.
- `~/pks/reference` — hub/MOC notes. Always add `moc` keyword.

## Command

```
denotecli create \
  --title "<title>" \
  --tags <comma,sep,keywords> \
  --dir <silo path> \
  [--content "<body text>"]
```

`--content -` reads from stdin.

## Required confirmation step

Before running `denotecli create`, present a plan:

```
Silo:     ~/pks/fleeting
Title:    <title>
Keywords: fleeting, research, ntnu
Body:     (first 3 lines of proposed content)
```

Ask: "Create this note? (y/n)". Do not proceed on ambiguous responses.

## Keyword discipline

Closed keyword set: `research code learn project lit perm fleeting ntnu
ous agenda moc meeting hub idea review`. Warn the user before
introducing a new keyword. Prefer existing ones.

## After creation

denotecli returns the full JSON of the created note. Share the path so
the user can jump to it. If they want to edit further, suggest opening
in Emacs (where denote bindings are available).
