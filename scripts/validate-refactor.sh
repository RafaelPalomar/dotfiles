#!/usr/bin/env bash
# Fast syntax/evaluation gate for the entelequia configuration.
#
# Evaluates every system machine file, every home machine file, and every VM
# config through the pinned channels (channels-lock.scm), asserting that each
# produces the expected record type.  This is tier 1 of the testing workflow:
# it catches module errors, unbound variables, and record-type mistakes in
# ~a minute, without building anything.
#
# Usage: ./scripts/validate-refactor.sh [--fast]
#   --fast   skip time-machine and use the ambient guix (faster, but channel
#            modules like sops-guix may be missing -> failures degrade to warn)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[1;33m'; NC='\033[0m'
pass() { echo -e "${GREEN}✓${NC} $1"; }
fail() { echo -e "${RED}✗${NC} $1"; failures=$((failures + 1)); }
warn() { echo -e "${YELLOW}⚠${NC} $1"; }

failures=0

if [[ "${1:-}" == "--fast" ]]; then
    GUIX=(guix)
else
    GUIX=(guix time-machine -C channels-lock.scm --)
fi

# eval_file FILE PREDICATE LABEL
# Loads FILE (whose trailing value must satisfy PREDICATE, e.g.
# operating-system? / home-environment?) under -L PROJECT_ROOT.
eval_file() {
    local file=$1 predicate=$2 label=$3
    local out
    out=$("${GUIX[@]}" repl -q -L . 2>&1 <<EOF
(use-modules (gnu) (gnu home))
(let ((value (primitive-load "$file")))
  (if ($predicate value)
      (display "ENTELEQUIA-VALIDATE-OK")
      (begin (display "ENTELEQUIA-VALIDATE-WRONG-TYPE") (exit 1))))
EOF
    )
    if grep -q "ENTELEQUIA-VALIDATE-OK" <<<"$out"; then
        pass "$label"
    else
        fail "$label"
        # Show the first real error line for quick diagnosis
        grep -m1 -E "error|Unbound|exception|WRONG-TYPE" <<<"$out" | sed 's/^/    /'
    fi
}

echo "== 1. Core library modules =============================="
out=$("${GUIX[@]}" repl -q -L . 2>&1 <<'EOF'
,use (entelequia lib records)
,use (entelequia lib helpers)
,use (entelequia system lib common-packages)
,use (entelequia system lib common-services)
,use (entelequia system lib security-hardening)
,use (entelequia home services desktop-suite)
,use (entelequia home services server-suite)
(display "ENTELEQUIA-VALIDATE-OK")
EOF
)
if grep -q "ENTELEQUIA-VALIDATE-OK" <<<"$out"; then
    pass "core lib/suite modules load"
else
    fail "core lib/suite modules load"
    grep -m1 -E "error|Unbound|exception" <<<"$out" | sed 's/^/    /'
fi
echo

echo "== 2. System machine configurations ======================"
for file in entelequia/system/machines/*.scm; do
    # Skip non-OS helper modules living alongside machines
    case "$(basename "$file")" in
        datalocker-udev-rules.scm) continue ;;
    esac
    eval_file "$file" "operating-system?" "$(basename "$file" .scm)"
done
echo

echo "== 3. Home machine configurations ========================"
for file in entelequia/home/machines/*.scm; do
    eval_file "$file" "home-environment?" "$(basename "$file" .scm)"
done
echo

echo "== 4. VM configurations =================================="
for file in entelequia/system/vms/*.scm; do
    eval_file "$file" "operating-system?" "$(basename "$file" .scm)"
done
echo

echo "=========================================================="
if [[ $failures -eq 0 ]]; then
    echo -e "${GREEN}Validation: PASSED${NC}"
    echo "Next: guix time-machine -C channels-lock.scm -- system build -L . \\"
    echo "        entelequia/system/machines/<machine>.scm --dry-run"
else
    echo -e "${RED}Validation: FAILED (${failures} file(s))${NC}"
    exit 1
fi
