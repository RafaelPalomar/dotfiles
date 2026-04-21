# User-level Claude Code instructions

Global instructions that apply in every project directory.  A
project-specific `CLAUDE.md` at the repo root overrides these.  Per-silo
`CLAUDE.md` files inside `~/pks/` layer on when you are working there.

## Personal Knowledge System (PKS)

The user maintains a function-based Zettelkasten at `~/pks/` (symlinked
into `~/Nextcloud/PKS/`) with silos: `fleeting/`, `permanent/`,
`literature/`, `projects/`, `reference/`, `review-queue/`, and
`library/` for raw material (PDFs, scans, `references.bib`).

Notes use the Denote filename convention:

```
YYYYMMDDTHHMMSS--slug__kw1_kw2.org
```

Closed keyword vocabulary (warn before extending):
`_research _code _learn _project _lit _perm _fleeting _ntnu _ous
_agenda _moc _meeting _hub _idea _review`.

### Tools available

- `denotecli` — structural queries (search / read / graph / tags /
  timeline / create / rename). Prefer this over `rg` on filenames.
  Use `--dirs ~/pks/<silo>` to scope searches.
- PKS Claude Code skills (`pks-*.md` under `~/.claude/skills/`) — use
  these as the primary entry points; they encode safety rules.

### Safety rules (always)

1. **Never regenerate denote IDs.** They are load-bearing for
   backlinks. Use `--keep-id` on any rename.
2. **Never bulk-delete or bulk-rename.** Single-note operations only.
3. **Always confirm** before creating, renaming, or moving notes.
4. **Default silo is `fleeting/`** — new notes land there unless the
   user specifies otherwise.
5. **Memory vs PKS**: ephemeral session context → auto-memory; durable
   knowledge → PKS project/permanent notes. Never duplicate full
   content in both — memory holds a short pointer to the PKS note ID.

## Project-awareness workflow

At session start in a new working directory, run `pks-project-context`
(the skill handles detection + routing):

1. Derive project name from `basename $PWD`.
2. `denotecli search "$proj" --dirs ~/pks/projects --tags project --max 1`
   to check if the project has a registered denote note.
3. **Registered** — load the note (`denotecli read <ID> --dirs
   ~/pks/projects --outline`) and carry Status, Log, Architecture,
   References as implicit session context. Do not re-announce on every
   turn.
4. **Unregistered** — **ask once per session**: "This project
   (`$proj`) isn't in your PKS. Want me to register it? (Worth it for
   sustained projects; skip for one-offs.)"
   - On **yes**: invoke `pks-project-register` which creates
     `~/pks/projects/<ID>--<slug>__project_agenda_<domain>.org` from
     the denote project template.
   - On **no**: remember the decline for this session only. Do not
     nag. Do not persist a "never register" marker.
5. **Trivial sessions** (user asked for `git log`, `ls`, a single
   command, a one-shot question) — skip the registration prompt
   entirely.  Only prompt when about to make substantive changes.

## Log-worthy events

During substantive work in a registered project, offer
`pks-project-log --id <ID> --summary "…"` when one of these occurs:

- Architectural decision with explicit justification.
- Rejected approach — document why, saves future re-deliberation.
- Pattern crystallising across multiple touchpoints.
- Discovery that changes how the project is understood.
- Non-obvious constraint or invariant revealed.

**Not log-worthy**: routine task completions, commit messages, linter
fixes, trivial refactors. Git history carries those.

Each log entry: one line + 1-3 sentences of context + optional
`[[denote:ID]]` to a permanent/literature note. User confirms before
append.

## Memory coordination

When logging to the PKS, consider whether a companion auto-memory
entry is warranted:

- **Feedback**: user correction or confirmation of approach → memory
  (`feedback_*.md`), plus PKS Log if it's a durable architectural
  decision.
- **Project state**: current blockers, in-flight decisions → memory
  (`project_*.md`) with a **pointer** to the PKS note, not a
  duplicate.
- **Reference**: pointers to external systems (Linear, Grafana) →
  memory (`reference_*.md`).
- **User profile**: role, preferences → memory (`user_*.md`).

PKS holds full context + reasoning. Memory holds short, quickly-indexed
pointers.

## Interaction style

- Terse by default — the user reads diffs, no trailing summaries of
  routine changes.
- Present exploratory questions as 2-3 sentence recommendations, not
  decided plans.
- Prefer rebase over merge (linear history preference).
- Never skip hooks (--no-verify) or force-push without explicit ask.
