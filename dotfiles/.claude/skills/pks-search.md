---
name: pks-search
description: Search the user's Denote-based PKS at ~/pks via denotecli. Use for structural queries — by tag/silo/title/age/ID. Returns JSON. Read-only. Prefer this over ripgrep for filename-level queries. Chain with pks-read for full-content reads.
---

# pks-search

Query the function-based Zettelkasten at `~/pks` using `denotecli`.

## Silo paths

```
fleeting   = ~/pks/fleeting
permanent  = ~/pks/permanent
literature = ~/pks/literature
projects   = ~/pks/projects
reference  = ~/pks/reference
review     = ~/pks/review-queue
legacy     = ~/Notes/Work-legacy
all        = ~/pks (recursive; excludes review-queue + legacy unless asked)
```

## Command forms

All commands emit JSON by default. Use `jq` or parse directly.

| Intent | Command |
|---|---|
| Title/body substring | `denotecli search <query> --dirs <path>` |
| Title-only | `denotecli search <query> --dirs <path> --title-only` |
| Tag filter | `denotecli search "" --dirs <path> --tags tag1,tag2` |
| Multiple silos | `denotecli search <query> --dirs ~/pks/fleeting,~/pks/permanent` |
| All PKS silos | `denotecli search <query> --dirs ~/pks` |
| Max results | append `--max 50` (default 20) |
| Headings search | `denotecli search-headings <query> --dirs <path> [--level N]` |
| Full content | `denotecli search-content <query> --dirs <path> [--matches 3]` |
| All tags (faceted) | `denotecli tags --dirs <path> [--top N]` |
| Recent activity | `denotecli day --dirs <path>` (today) |
| Monthly | `denotecli timeline-journal --month 2026-04 --dirs <path>` |

## Translation examples

- "find notes tagged _project and _agenda"
  → `denotecli search "" --dirs ~/pks/projects --tags project,agenda`
- "notes about tensor cores"
  → `denotecli search "tensor cores" --dirs ~/pks`
- "MOCs"
  → `denotecli search "" --dirs ~/pks/reference --tags moc`
- "what was I writing last week"
  → `denotecli day $(date -d '7 days ago' +%F) --dirs ~/pks --days-ago 7`

## Output shaping

Parse JSON and present a compact table: `ID  TITLE  TAGS  PATH`.
Cap at 50 rows; offer "refine?" if truncated.

## Safety

Read-only. Never invoke `denotecli create`, `rename`, or `rename-tag`
from this skill. Hand off to `pks-create`, `pks-promote`, or
`pks-project-register` for mutations.
