---
description: "Read-only daily or weekly PKS review. Lists stale fleeting notes (>14 days), active project notes (tagged _agenda), stale MOCs (>30 days), and stale review-queue items (>30 days) so the user can triage. Never mutates — suggests promotions/archives via pks-promote or manual edits."
---
# pks-daily-review

Four-section passive report. The SessionStart hook surfaces a compact
one-line digest like `8 active _agenda projects · …` so the user can
see backlog volume without opening the note.

## Commands

### 1. Stale fleeting (triage candidates)

```
denotecli search "" \
  --dirs ~/pks/fleeting \
  --max 100
```

Parse JSON, filter entries where `date` is older than 14 days from
today. Present as:

```
Stale fleeting (>14d):
  <ID>  <title>  <tags>
  ...
Suggested actions:
  - Promote to permanent with pks-promote
  - Archive (manual: move to ~/pks/review-queue)
  - Delete (manual only; never from AI)
```

### 2. Active projects

```
denotecli search "" --dirs ~/pks/projects --tags agenda
```

For each: show Status from the note (use `pks-read --outline` to
extract the first heading's content).

### 3. Stale MOCs

```
denotecli search "" --dirs ~/pks/reference --tags moc --max 50
```

Filter to entries not modified in 30 days. These are hubs that may
need re-curation after new permanent notes were created.

### 4. Stale review-queue (triage backlog)

```
denotecli search "" --dirs ~/pks/review-queue --max 200
```

The review-queue silo holds notes awaiting triage (legacy imports,
items the user wasn't sure where to place). Filter to entries whose
mtime is older than 30 days — those have sat without action long
enough to warrant a decision.

For each present:
  <ID>  <title>  <tags>  <days-since-mtime>

Suggested actions per item:
  - Promote (move to fleeting/permanent with pks-promote).
  - Archive (move to a dedicated archive silo if the user has one).
  - Delete (manual only; never from AI).

The skill output is a list, not a recommendation per item — the user
chooses what to do with each.

Note: items in review-queue may not yet carry `_review` in their
filename keywords. The query above does not filter by tag; it scans
the entire silo. If the user later tags items with `_review`, you can
add `--tags review` to narrow the scan.

## Safety

Read-only. This skill NEVER writes, renames, or deletes. Outputs a
plain-language review the user can act on.
