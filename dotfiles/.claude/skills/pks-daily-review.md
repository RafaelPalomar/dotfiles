---
name: pks-daily-review
description: Read-only daily or weekly PKS review. Lists stale fleeting notes (>14 days), active project notes (tagged _agenda), and stale MOCs (>30 days) so the user can triage. Never mutates — suggests promotions/archives via pks-promote or manual edits.
---

# pks-daily-review

Three-section passive report.

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

## Safety

Read-only. This skill NEVER writes, renames, or deletes. Outputs a
plain-language review the user can act on.
