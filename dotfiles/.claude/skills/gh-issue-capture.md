---
name: gh-issue-capture
description: Append a GitHub issue or PR reference to a PKS project's `* Log` section. Use when the user asks to "log this issue", "add this PR to <project>", "track issue #N from <repo>", or wants an open GitHub conversation reflected in their project's chronological record. Wraps `gh` (read-only) + the `gh-issue-to-log` shell script. Mirrors the mu4e `m a` (Action Item → project Log) capture path — for GitHub instead of mail.
---

# gh-issue-capture

The agent counterpart to `m a` (Action Item → project Log) for GitHub
issues and PRs.  Use when the user wants an issue/PR reflected in the
chronological Log of its corresponding PKS project.

## When to use

- *"Log issue #142 from Slicer/SlicerSOFA."*
- *"Add this PR to project-monk-system."*
- *"Track issue #7 from MONK-system in PKS."*
- *"Capture this GitHub thread to project-paladin."*

Do NOT use when:

- The user wants to *capture as durable knowledge* — that's
  [[file:./mail-to-fleeting.md][mail-to-fleeting]] or `pks-create` for
  a fleeting denote.  Issues are URL-addressable; the conversation
  lives on GitHub, the Log just references it.
- The repo doesn't map to a registered PKS project — register the
  project first via `pks-project-register` (or `C-c c P` interactively),
  then capture.

## Flow

1. Identify `<owner/repo>` and `<issue#>`.  The user usually supplies
   both; otherwise extract from a URL like
   `https://github.com/Slicer/SlicerSOFA/issues/142`.

2. Verify the project mapping.  Default mappings in
   `~/.config/pks-gh/repo-project-map`:

   ```
   RafaelPalomar/dotfiles               → entelequia (20260421T194720)
   Slicer/SlicerSOFA                    → project-slicer-sofa (20260507T130419)
   SystoleOS/guix-systole               → project-guix-systole (20260422T210014)
   OUH-MESHLab/SlicerHyperProbe         → project-hyperprobe (20260507T130440)
   OUH-MESHLab/TCIADataAugmentation     → project-slicer-sofa (20260507T130419)
   OUH-MESHLab/IVSVista                 → project-ivs-infrastructure (20260427T130819)
   OUH-MESHLab/MONK-system              → project-monk-system (20260430T092504)
   ```

   If the repo isn't mapped, ask the user which project (or whether
   they want to register a new one first).

3. Run the script:

   ```bash
   gh-issue-to-log <owner/repo> <issue#>
   # Or with explicit override:
   gh-issue-to-log <owner/repo> <issue#> --project <denote-id>
   # With status annotation:
   gh-issue-to-log <owner/repo> <issue#> --note "WAITING upstream review"
   ```

   The script:
   - Reads issue/PR metadata via `gh` (number, title, state, URL,
     labels) — auto-detects PR vs issue.
   - Resolves the project file from the denote ID.
   - Appends a dated bullet under the project's `* Log` heading:

     ```
     - 2026-05-07 :: ISSUE #142 SlicerSOFA: TCIA dataset loading regression [OPEN] [bug, high-priority] — [[https://github.com/Slicer/SlicerSOFA/issues/142][gh thread]]
     ```

4. Confirm to the user with the project file path returned by the
   script.  Don't paste the entire Log; the user can open the path.

## Hard rules

- `gh` is invoked **read-only**.  Never call `gh issue close`,
  `gh issue edit`, `gh issue comment`, `gh pr merge`, `gh pr close`,
  `gh pr review`, or any write subcommand.  All write actions belong
  on a human keystroke (the GitHub web UI or a deliberate `gh`
  invocation by the user).
- Don't write more than one Log entry per invocation — the bulk
  variant is `gh-morning-triage` (separate skill, not yet built).
- Don't fabricate label or state strings — read them from the `gh`
  JSON output.
- If the project's `* Log` heading is missing, error with a clear
  message; don't silently create one.
- The bullet's `[[<url>][gh thread]]` is load-bearing — it's how the
  user re-finds the conversation later.  Keep the URL exactly as
  returned by `gh`.

## After capture

If the issue / PR carries an action item the user owns:

- Suggest also adding it as a `** TODO` under the project's `* Next
  actions` heading (the Log is chronology; Next actions is the
  current todo list).
- For waiting-on-someone-else items: pass `--note "WAITING <who>
  <what>"` so the bullet is grep-able alongside `WAITING` markers
  from the mail flow.

## Why this shape

GitHub issues are already URL-addressable, threaded, and live on
github.com.  The Log entry just *references* the conversation; the
agent should not duplicate the issue body into PKS (mirrors the
mail rule against pasting message bodies).  This keeps the project
note's Log skimmable: one bullet = one event = one round-trippable
link.
