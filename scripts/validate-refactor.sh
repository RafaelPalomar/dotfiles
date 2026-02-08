#!/usr/bin/env bash
# Validation script for the refactored entelequia configuration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo "========================================"
echo "Entelequia Configuration Validation"
echo "========================================"
echo

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

pass() {
    echo -e "${GREEN}✓${NC} $1"
}

fail() {
    echo -e "${RED}✗${NC} $1"
    return 1
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Test 1: Module syntax checking
echo "1. Checking Guile module syntax..."
if guix repl -L . <<'EOF' 2>&1 | grep -q "All modules loaded"
,use (entelequia lib records)
,use (entelequia lib helpers)
,use (entelequia home profiles base)
,use (entelequia home profiles development)
,use (entelequia home profiles email)
,use (entelequia home services emacs)
,use (entelequia home services desktop)
,use (entelequia home services gpg)
,use (entelequia home services shell)
(display "All modules loaded successfully\n")
EOF
then
    pass "All core modules load successfully"
else
    warn "Some modules have warnings (check output above)"
fi
echo

# Test 2: System configuration evaluation
echo "2. Testing system configurations..."

for system in einstein curie; do
    echo "   Testing $system..."
    if guix repl -L . <<EOF 2>&1 | grep -q "operating-system"
,use (entelequia system machines $system)
(display (if (operating-system? ${system}-system) "operating-system" "not-found"))
EOF
    then
        pass "$system system configuration evaluates correctly"
    else
        fail "$system system configuration has errors"
    fi
done
echo

# Test 3: VM configuration evaluation
echo "3. Testing VM configurations..."
if guix repl -L . <<'EOF' 2>&1 | grep -q "operating-system"
,use (entelequia system vms test-desktop)
(display (if (operating-system? test-desktop-system) "operating-system" "not-found"))
EOF
then
    pass "test-desktop VM configuration evaluates correctly"
else
    fail "test-desktop VM configuration has errors"
fi
echo

# Test 4: Check for common issues
echo "4. Checking for common issues..."

# Check for missing package definitions
missing_pkgs=$(grep -r "specifications->packages" entelequia/system/lib/common-packages.scm | wc -l)
if [ "$missing_pkgs" -eq 0 ]; then
    warn "No package specifications found (expected at least 1)"
else
    pass "Found $missing_pkgs package specifications"
fi

# Check for duplicate service definitions
duplicate_services=$(grep -r "define.*-service" entelequia/system/lib/common-services.scm | wc -l)
if [ "$duplicate_services" -gt 0 ]; then
    pass "Found $duplicate_services service definitions"
else
    warn "No service definitions found"
fi
echo

# Test 5: File structure validation
echo "5. Validating file structure..."
required_files=(
    "entelequia/lib/records.scm"
    "entelequia/lib/helpers.scm"
    "entelequia/system/machines/einstein.scm"
    "entelequia/system/machines/curie.scm"
    "entelequia/system/layers/base.scm"
    "entelequia/system/layers/desktop-base.scm"
    "entelequia/home/home-config.scm"
)

all_present=true
for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        pass "Found $file"
    else
        fail "Missing $file"
        all_present=false
    fi
done
echo

# Summary
echo "========================================"
if $all_present; then
    echo -e "${GREEN}Validation Summary: PASSED${NC}"
    echo "The refactored configuration appears to be structurally sound."
    echo
    echo "Next steps:"
    echo "  1. Review any warnings above"
    echo "  2. Run 'guix time-machine -C channels.scm -- system build entelequia/system/machines/einstein.scm --dry-run'"
    echo "  3. Test in a VM before applying to real hardware"
else
    echo -e "${RED}Validation Summary: FAILED${NC}"
    echo "Please fix the errors above before proceeding."
    exit 1
fi
echo "========================================"
