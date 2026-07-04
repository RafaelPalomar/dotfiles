---
description: "Read-only Sunday-cadence PKS review. Surfaces project-level staleness the daily review misses — stale projects (mtime >14d), stale _agenda notes anywhere in PKS (>14d), and stale review-queue items (>30d). Writes a denote-named fleeting note for the user to act on Monday. Never mutates."
---
# pks-weekly-review

Three-section project-level digest for Sunday evening. Complements
`pks-daily-review` (which is note-level). Read-only — proposes, never
acts.

## When to use

- Cron-driven invocation Sunday 19:00 (set up via CronCreate).
- On-demand: *"weekly review"*, *"what's getting stale?"*

## Three sections

### 1. Stale projects (mtime >14 days)

```
denotecli search "" --dirs ~/pks/projects --tags project --max 200
```

Filter to `mtime > 14 days`. Projects whose file hasn't been
touched in 2 weeks — Status section is probably out of date,
deserves a refresh or explicit "paused" annotation.

### 2. Stale `_agenda` notes (any silo, mtime >14 days)

```
denotecli search "" --dirs ~/pks --tags agenda --max 200
```

Filter to `mtime > 14 days`. The daily review lists *which* notes
are agenda-tagged but doesn't filter by staleness — this catches
the ones that are sitting.

### 3. Stale review-queue (mtime >30 days)

```
denotecli search "" --dirs ~/pks/review-queue --max 200
```

Same scan as the daily review's section 4. Repeated here so the
weekly digest is self-contained — Sunday review can be done from
this file alone.

## Output

Single fleeting note:

  `~/pks/fleeting/<YYYYMMDD>T190000--weekly-review-<ISO-week>__review.org`

Filename uses ISO week (e.g. `2026-W19`) so each weekly digest is
distinct and discoverable. Re-running the same Sunday overwrites.

## Suggested actions per section

- Stale project → `pks-project-log` (Mode A) with current Status,
  or rename keywords to drop `_agenda` if the project is paused.
- Stale `_agenda` note → archive (move to review-queue or remove
  the `_agenda` keyword via `denotecli rename`).
- Stale review-queue → promote (`pks-promote`), archive, or delete
  manually.

The skill output is a list, not a recommendation per item — the user
chooses what to do with each.

## Safety

Read-only. Never writes, renames, or deletes any note other than the
weekly-review fleeting note it generates. All proposals require user
confirmation through other skills (`pks-promote`, `pks-project-log`)
or manual edits.

## Shell-script fallback

`~/.local/bin/pks-weekly-review` produces the same digest without an
LLM. Use when the agent is offline or the user only wants the
structural view.
