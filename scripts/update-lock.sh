#!/usr/bin/env bash
# Regenerate channels-lock.scm from channels.scm.
#
# channels.scm is the INTENT file (names, URLs, branches, introductions);
# channels-lock.scm is the LOCK (exact commits) used by every deploy /
# reconfigure via guix time-machine.  The lock was historically hand-edited,
# which produced drift (phantom channels, truncated hashes).  This script is
# the only supported way to move the pins:
#
#   1. time-machine resolves channels.scm to the latest commit of every
#      branch (authenticating channels that carry an introduction), then
#   2. `describe -f channels` inside that environment emits the fully
#      pinned channel list, which becomes the new lock.
#
# Review the diff and commit channels.scm + channels-lock.scm together.
#
# Usage: ./scripts/update-lock.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

new_lock=$(mktemp channels-lock.scm.XXXXXX)
trap 'rm -f "$new_lock"' EXIT

echo "Resolving channels.scm to latest commits (this builds the new guix)..."
guix time-machine -C channels.scm -- describe -f channels > "$new_lock"

# Sanity: the regenerated lock must cover exactly the channels declared in
# the intent file — catches typos and half-finished channel additions.
NEW_LOCK="$new_lock" guix repl -q <<'EOF'
(use-modules (guix channels) (srfi srfi-1))
;; NB: one top-level form, wrapped in catch — channels.scm carries a
;; define-module that switches the REPL module mid-load, which breaks
;; helpers defined in separate top-level forms.
(catch #t
  (lambda ()
    (let ()
      (define (channel-names file)
        (sort (map (compose symbol->string channel-name) (primitive-load file))
              string<?))
      (let* ((intent  (channel-names "channels.scm"))
             (lock    (channel-names (getenv "NEW_LOCK")))
             (missing (lset-difference equal? intent lock))
             (extra   (lset-difference equal? lock intent)))
        (unless (and (null? missing) (null? extra))
          (for-each (lambda (n) (format #t "declared but not in new lock: ~a~%" n)) missing)
          (for-each (lambda (n) (format #t "in new lock but not declared: ~a~%" n)) extra)
          (exit 1)))))
  (lambda args
    (if (equal? (car args) 'quit)
        (apply throw args)              ; let (exit 1) through
        (begin (format #t "lock-sanity-error: ~s~%" args) (exit 1)))))
EOF

mv "$new_lock" channels-lock.scm
trap - EXIT

echo
echo "Pin movement:"
git --no-pager diff --stat channels-lock.scm
git --no-pager diff channels-lock.scm | grep -E "^[-+] +\(commit" | head -30 || true
echo
echo "Next: review the diff, run ./scripts/validate-refactor.sh, then commit"
echo "channels.scm and channels-lock.scm together."
