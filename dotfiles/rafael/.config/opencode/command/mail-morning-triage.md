---
description: "Cadence-driven bulk triage of recent mail. Reads the last 24h of unread, untriaged threads via notmuch-agent and writes a single fleeting note (~/pks/fleeting/<date>--mail-triage__review.org) with subject, sender, 2-line summary, proposed classification, and a ready-to-confirm action stub for each thread. Tags processed threads `agent-seen` so they don't reappear. Cap: top 20 by recency. Use on demand or wire to a CronCreate at 08:30."
---
# mail-morning-triage

The morning brief for mail — turn an unread inbox into a list of
proposals the user can review once and act on. Complements the
on-demand single-thread `mail-to-fleeting` (fix #4) and the human
capture path (mu4e `C-c c m e`).

## When to use

- *"Triage my inbox."*
- *"What new mail needs attention?"*
- Cron-driven invocation: scheduled at 08:30 daily after the user's
  typical sync window.

The user must have run `sync-mail` themselves first (the agent cannot
sync). If the agent's notmuch DB is stale, output reflects the last
sync time — that's fine for retrospective triage.

## What this skill does NOT do

- Send mail. Drafted replies stay as proposals; sending is a human
  `C-c C-c` in mu4e.
- Read the Sensitive folder (it isn't indexed in the agent's DB).
- Re-process threads it has already seen (filtered by `agent-seen`
  tag in the agent's Xapian DB — local, doesn't touch main notmuch).
- Run when the LLM is unavailable. There is no shell-script fallback
  (no useful classification without an LLM). Offline mode = manual
  mu4e triage as before.

## Flow

1. Query untriaged unread threads:

   ```bash
   notmuch-agent search \
     --format=json \
     --output=summary \
     --sort=newest-first \
     'tag:inbox and not tag:agent-seen and not tag:flagged and date:1d..'
   ```

   Cap at 20 results. If there are more, note the overflow at the
   top of the digest ("12 more older than 24h — invoke
   `mail-morning-triage --since 7d` for a wider window").

2. For each thread, fetch the root message:

   ```bash
   notmuch-agent show thread:<THREAD_ID> --format=json
   ```

   Extract Subject, From, Date, body excerpt (first ~200 chars).

3. Classify the thread into ONE of:

   - `reply`        — explicit question or request needing your answer.
   - `fleeting`     — durable knowledge worth capturing (decision,
                      policy clarification, important context).
   - `project-log`  — action item or waiting-for tied to a registered
                      project. Pick the project from the title /
                      sender / body hints.
   - `no-action`    — informational, FYI, newsletter, automated.

   When uncertain, prefer `no-action`. Do not over-propose.

4. Compose 2-line summary per thread: who is asking what, what
   decision/action is implied. Keep summaries factual; don't infer
   urgency.

5. Compose a proposed action stub matching the classification:

   - reply        → `mail-draft --account ... --in-reply-to '<msg-id>' ...`
                    plus a 2–4 sentence draft body.
   - fleeting     → `mail-to-fleeting <msg-id>`
                    plus the keywords you'd use.
   - project-log  → `pks-project-log` invocation suggesting Mode B
                    (Action Item or Waiting For) on the relevant
                    project.
   - no-action    → `notmuch-agent tag +agent-seen id:<msg-id>`
                    (the no-op acknowledgement).

6. Write a single fleeting note:

   Path: `~/pks/fleeting/<YYYYMMDD>T083000--mail-triage__review.org`

   Filename uses the date stamp so each day produces one digest;
   re-running the same day overwrites. Format:

   ```org
   #+title:      Mail triage <date>
   #+date:       [<date>]
   #+filetags:   :review:
   #+identifier: <YYYYMMDD>T083000

   * <Subject>  (reply | fleeting | project-log | no-action)
   ** From / Date / Msg-ID
   - From:    <sender>
   - Date:    <date>
   - Msg-ID:  <msg-id>
   ** Summary
   <2 lines>
   ** Proposed action
   #+begin_src bash
   <action stub>
   #+end_src

   * <next thread...>
   ```

7. After writing the file, tag every processed thread with
   `agent-seen` so the next run skips them:

   ```bash
   notmuch-agent tag +agent-seen <msg-id1> <msg-id2> ...
   ```

   This tag lives only in the agent's Xapian DB — does not pollute
   the user's main notmuch index or mu4e.

8. Print the path of the digest. The user reviews it interactively
   (in mu4e or Emacs), confirms or edits each proposed action, and
   either runs the stub or skips it.

## Cap and overflow

If more than 20 threads match the query, surface the cap at the top
of the digest with the suggested `--since 7d` / `--since 14d` widening.
Do not silently truncate.

## Hard rules

- Never call `sync-mail`, `mbsync`, or any sync tool — the agent does
  not refresh its own index. If the user wants fresher mail, ask them
  to sync.
- Never produce more than 20 thread entries per digest. The cap is
  the point — it makes the morning brief skimmable.
- Never include full message bodies in the digest. The 2-line
  summary is the contribution; the msg-id link is the canonical
  reference.
- Never auto-execute any of the proposed action stubs. They are
  proposals; the user runs them after review.
- Tag `agent-seen` only AFTER the digest is written successfully.
  If the file write fails, leave the tags untouched so the next run
  picks up the same threads.

## Trigger setup (CronCreate)

Schedule as a daily 08:30 trigger via CronCreate. Prompt:

> Run the `mail-morning-triage` skill. Read up to 20 untriaged threads
> from the last 24h, classify each, and write the digest to today's
> fleeting note. Do not converse — produce the file and report only
> its path.
