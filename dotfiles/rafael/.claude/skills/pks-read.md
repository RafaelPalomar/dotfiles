---
name: pks-read
description: Fetch the full content or outline of a specific denote note in ~/pks by ID. Use after pks-search has returned a candidate ID, or when the user gives an ID explicitly. Supports offset/limit for paging and --outline for heading-only view.
---

# pks-read

Read a specific denote note by its timestamp identifier.

## Commands

| Intent | Command |
|---|---|
| Full content | `denotecli read <ID> --dirs <path>` |
| Outline only | `denotecli read <ID> --dirs <path> --outline` |
| Paged | `denotecli read <ID> --dirs <path> --offset N --limit N` |
| Graph neighbours | `denotecli graph <ID> --dirs <path>` |

## Typical flow

1. `pks-search` returned one or more candidates.
2. User picks a candidate (or you pick the top hit if unambiguous).
3. Fetch the outline first for large notes; fetch full content only
   when needed.
4. For link-aware context, `denotecli graph <ID>` returns backlinks and
   forward links.

## Silo scoping

When you know the silo, pass the tight path (`~/pks/projects`). When
you don't, pass `~/pks` and let denotecli find it across silos.

## Output

`denotecli read` emits JSON: `{id, title, tags, date, path, content,
links}`. `content` is the raw org text. `links` is null or a list of
referenced denote IDs.

## Safety

Read-only. For editing, hand to `pks-create` (new note) or instruct
the user to open the `path` in their editor.
