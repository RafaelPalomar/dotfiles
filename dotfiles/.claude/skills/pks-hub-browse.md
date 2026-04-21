---
name: pks-hub-browse
description: List or open MOC (Map of Content) / hub notes in ~/pks/reference. Use when the user wants to navigate topics, see what's curated, or find where to add a new permanent note. Read-only.
---

# pks-hub-browse

Hubs (`_moc`) in `~/pks/reference/` are curated landing pages. Each
hub names a topic and lists/queries the notes within it.

## Commands

### List all hubs

```
denotecli search "" --dirs ~/pks/reference --tags moc
```

### Find hub for topic

```
denotecli search "<topic>" --dirs ~/pks/reference --tags moc --title-only
```

### Open a hub

Use `pks-read` with the returned ID and `--outline` to see structure,
or full content if small.

### Suggest related hubs for a note

If user is working in a permanent note tagged `_research _code`:

```
denotecli search "" --dirs ~/pks/reference --tags moc,research
denotecli search "" --dirs ~/pks/reference --tags moc,code
```

Return both sets; user picks which hub(s) the note should link to or
be linked from.

## Safety

Read-only. Creation of new hubs is user-driven via Emacs
`C-c n h` (`my-pks-capture-hub`). If user asks to create a hub from
this skill, hand off to `pks-create` with `--dir ~/pks/reference
--tags moc,hub,<topic>`.
