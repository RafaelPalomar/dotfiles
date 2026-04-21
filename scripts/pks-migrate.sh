#!/usr/bin/env bash
# pks-migrate.sh — one-shot migration of ~/Notes into ~/Nextcloud/PKS/.
# Creates the function-based Zettelkasten silo tree, symlinks ~/pks into
# it, moves ~/Notes/Work to Work-legacy (read-only), and parks
# ~/Notes/Personal into the review queue for gradual promotion.
# Idempotent.  Pass --dry-run to preview.  See plan:
#   /home/rafael/.claude/plans/we-are-going-to-cheerful-clarke.md
set -euo pipefail

DRY_RUN=0
VERBOSE=0
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    --verbose|-v) VERBOSE=1 ;;
    -h|--help)
      sed -n '2,8p' "$0" | sed 's/^# //; s/^#//'
      exit 0
      ;;
    *)
      echo "[pks] unknown argument: $arg" >&2
      exit 2
      ;;
  esac
done

log() { [ "$VERBOSE" = 1 ] && echo "[pks] $*" >&2 || true; }
run() {
  log "$*"
  if [ "$DRY_RUN" = 0 ]; then
    eval "$@"
  fi
}

NC="$HOME/Nextcloud/PKS"
PKS="$HOME/pks"
NOTES="$HOME/Notes"
SILOS=(fleeting permanent literature projects reference review-queue
       library library/papers library/books library/data library/scans)

# 0. Preconditions
if [ ! -d "$HOME/Nextcloud" ]; then
  echo "[pks] ~/Nextcloud is not present — start the Nextcloud client" >&2
  echo "[pks] and let it sync once before running this migration." >&2
  exit 2
fi

# 1. Create silo tree inside Nextcloud
for s in "${SILOS[@]}"; do
  run "mkdir -p \"$NC/$s\""
done

# 2. Ensure ~/pks → ~/Nextcloud/PKS symlink
if [ ! -e "$PKS" ]; then
  run "ln -s \"$NC\" \"$PKS\""
elif [ -L "$PKS" ]; then
  log "~/pks already symlink — leaving in place"
elif [ -d "$PKS" ]; then
  echo "[pks] $PKS exists as a real directory." >&2
  echo "[pks] Move its contents into $NC manually, remove $PKS," >&2
  echo "[pks] then re-run this script." >&2
  exit 3
fi

# 3. Work → Work-legacy (read-only, still searchable; outside PKS)
if [ -d "$NOTES/Work" ] && [ ! -d "$NOTES/Work-legacy" ]; then
  run "mv \"$NOTES/Work\" \"$NOTES/Work-legacy\""
  run "chmod -R a-w \"$NOTES/Work-legacy\""
fi

# 4. Personal → review-queue (no reclassification; promote manually later)
if [ -d "$NOTES/Personal" ]; then
  # shellcheck disable=SC2046
  run "find \"$NOTES/Personal\" -mindepth 1 -maxdepth 1 -exec mv -t \"$PKS/review-queue/\" -- {} +"
  run "rmdir \"$NOTES/Personal\" 2>/dev/null || true"
fi

# 5. Seed empty bibliography
if [ ! -f "$PKS/library/references.bib" ]; then
  run "touch \"$PKS/library/references.bib\""
fi

# 6. Seed PKS root hub (once)
if ! ls "$PKS/reference/"*pks-root-hub*.org >/dev/null 2>&1; then
  HUB_ID="$(date -u +%Y%m%dT%H%M%S)"
  HUB_PATH="$PKS/reference/${HUB_ID}--pks-root-hub__moc_index.org"
  if [ "$DRY_RUN" = 0 ]; then
    cat > "$HUB_PATH" <<EOF
#+title:      PKS root hub
#+filetags:   :moc:index:
#+identifier: ${HUB_ID}

* Silos
- fleeting :: transient capture
- permanent :: atomic evergreens
- literature :: source-anchored notes (bibliography lives in library/)
- projects :: outcome-bound work (_agenda drives org-agenda)
- reference :: durable facts and MOCs (you are here)
- review-queue :: pre-classification inbox
- library :: raw material (PDFs, scans, datasets, references.bib)

* Topic hubs
# Add topical MOCs as [[denote:ID][Title]] links.
EOF
  else
    log "would seed $HUB_PATH"
  fi
fi

# 7. Seed per-silo CLAUDE.md templates if absent
seed_claude_md() {
  local path="$1"
  shift
  if [ ! -f "$path" ]; then
    if [ "$DRY_RUN" = 0 ]; then
      mkdir -p "$(dirname "$path")"
      cat > "$path" <<EOF
$*
EOF
    else
      log "would seed $path"
    fi
  fi
}

seed_claude_md "$PKS/CLAUDE.md" \
"# PKS — Personal Knowledge System

This tree is a function-based Zettelkasten managed with Denote.

## Silos
- fleeting/ : inbox; transient captures.  Default denote-directory.
- permanent/ : atomic evergreens (one claim per note).
- literature/ : source-anchored notes; paired with bibliography entries.
- projects/ : GTD source of truth; _agenda drives org-agenda.
- reference/ : hub / MOC notes (_moc keyword).
- review-queue/ : legacy notes awaiting manual promotion.
- library/ : raw material (PDFs, scans, datasets, references.bib).

## Keyword vocabulary (closed set — warn before extending)
_research _code _learn _project _lit _perm _fleeting _ntnu _ous
_agenda _moc _meeting _hub _idea _review

## Safety rules for AI agents
1. **Never regenerate denote IDs.**  IDs are load-bearing for backlinks.
2. **Never bulk-delete or bulk-rename.**  Single-note operations only.
3. **Always confirm** before creating, renaming, or moving notes.
4. **Use denotecli** for structural queries (tags, silos, graph), not
   raw ripgrep on filenames.
5. **Capture → fleeting**: new notes default to the fleeting silo.
6. **Promote, don't duplicate**: fleeting → permanent is a move, not a
   copy.

## Structural queries
- \`denotecli search <query> --dirs ~/pks/<silo> --tags t1,t2\`
- \`denotecli read <ID> --dirs ~/pks/<silo> [--outline]\`
- \`denotecli graph <ID> --dirs ~/pks/<silo>\`"

seed_claude_md "$PKS/projects/CLAUDE.md" \
"# projects/ — GTD source of truth

Each active project is ONE denote note in this silo.

## Note structure
\`\`\`
#+title:      PROJECT <name>
#+filetags:   :project:agenda:

* Status        — current state (updated on significant change)
* Next actions  — TODO tree with SCHEDULED/DEADLINE
* Log           — dated decisions, rejected approaches, patterns
* Architecture  — stable patterns characterising this project
* References    — [[denote:ID]] links into permanent/ and literature/
\`\`\`

## Keywords
Mandatory: _project _agenda.  Add domain keyword(s): _code, _research,
_ntnu, _ous, _learn.

## Log discipline — what to append
YES: architectural decisions with justification, rejected approaches,
patterns that crystallised, discoveries that change understanding.
NO: routine task completions, commit messages, minor fixes.  Git
handles those.

## AI agent guidance
- Load the note as session context when working in a registered project.
- On entering an unregistered project: **ask** the user whether to
  register.  Don't auto-register.
- Offer to append Log entries when log-worthy events occur."

seed_claude_md "$PKS/permanent/CLAUDE.md" \
"# permanent/ — atomic evergreen notes

## Rules
- **One claim per note.**  Title asserts the claim (\"Tensor cores
  amortise GEMM across half-precision matmuls\", not \"Tensor cores\").
- **Every permanent note should be linked from at least one other note
  or hub within a week** — otherwise reconsider whether it belongs
  here.
- Prefer [[denote:ID][title]] over file: links.
- When adding to a topic, update its MOC in reference/ if one exists."

seed_claude_md "$PKS/literature/CLAUDE.md" \
"# literature/ — source-anchored notes

Notes here summarise or engage with a specific source (paper, book,
talk, podcast).  They are paired with a bibliography entry in
library/references.bib via \`#+reference: @citekey\`.

## Typical flow
1. Add \`@article{...}\` entry to ~/pks/library/references.bib.
2. Drop PDF in ~/pks/library/papers/ (or \`citar-add-file-to-library\`).
3. In Emacs: \`C-c n r n\` (citar-open-notes) creates this literature
   note with \`#+reference: @citekey\`.
4. Read PDF, take notes, link out to permanent/ with [[denote:ID]].

## Required keywords
_lit (set by citar-denote automatically).

## Notes on denotecli
denotecli ignores library/ (no denote-format filenames there).  Use
citar from Emacs, or \`rg\` on references.bib for bibliographic
queries."

seed_claude_md "$PKS/library/CLAUDE.md" \
"# library/ — raw material

This silo holds NON-denote files — PDFs, scans, datasets, and the
central bibliography \`references.bib\`.

## Subdirectories
- papers/  : academic PDFs, named by citekey
- books/   : book PDFs / epubs
- data/    : datasets, supplementary files
- scans/   : scanned documents, images

## PDF naming convention
\`{AuthorYearTitle}.pdf\` matching the bibtex citekey.
\`citar-add-file-to-library\` automates rename-on-attach.

## references.bib
Single central file.  May be auto-exported from Zotero via Better
BibTeX, or hand-curated.  Do not version filenames by topic — keep
one file; slice by keywords in bib entries.

## For AI agents
- Use \`rg\` or \`bibtool\` to query references.bib.
- Never commit secrets into this directory (scans of IDs, etc. — if
  ever needed, use a separately encrypted store)."

echo "[pks] done."
