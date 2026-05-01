#!/usr/bin/env bash
# manage-deploy-keys — GPG auth subkey management for machine deployments
#
# Each deployment gets its own GPG [A] subkey used as its SSH deploy key.
# The master key is needed only when adding subkeys; it should be kept offline
# on the IronKey at all other times.
#
# Subkey name→keygrip associations are stored in ~/.gnupg/deploy-keys.conf.
#
# Usage:
#   manage-deploy-keys add <name> [<expire>]    Create auth subkey for a deployment
#   manage-deploy-keys list                     List subkeys with status + SSH pubkeys
#   manage-deploy-keys pubkey <name|keygrip>    Print SSH public key
#   manage-deploy-keys enable  <name|keygrip>   Enable subkey in sshcontrol
#   manage-deploy-keys disable <name|keygrip>   Disable subkey in sshcontrol

set -euo pipefail

MASTER_KEY_FP="${GPG_MASTER_KEY_FP:-6513C7248D7BECE2EC1BD34B70350DAD507FA72F}"
GNUPGHOME="${GNUPGHOME:-$HOME/.gnupg}"
SSHCONTROL="$HOME/.dotfiles/dotfiles/.gnupg/sshcontrol"
DEPLOY_KEYS_DB="$HOME/.dotfiles/dotfiles/.gnupg/deploy-keys.conf"

# ── Helpers ────────────────────────────────────────────────────────────────────

die()  { echo "error: $*" >&2; exit 1; }
info() { echo "==> $*"; }

require_master_key() {
    gpg --list-secret-keys "$MASTER_KEY_FP" 2>/dev/null \
        | grep -qE '^sec[[:space:]]' \
        || die "master key is offline (sec#). Import from IronKey first:
  gpg --import /media/ironkey/gpg/master-key.asc.gpg"
}

# Emit lines: FINGERPRINT KEYGRIP CREATED EXPIRES
# for every [A] auth subkey on the master key.
get_auth_subkeys() {
    gpg --with-colons --with-keygrip --list-keys "$MASTER_KEY_FP" 2>/dev/null \
    | awk -F: '
        /^(sub|ssb):/ {
            if (index($12, "a") > 0) {
                in_auth = 1; fp = ""; grip = ""
                created = $6; expires = $7
            } else { in_auth = 0 }
        }
        /^fpr:/ && in_auth && fp   == "" { fp   = $10 }
        /^grp:/ && in_auth && fp   != "" { grip = $10
            print fp, grip, created, expires
            in_auth = 0
        }
    '
}

# DB helpers: KEYGRIP NAME DATE (space-separated, one per line, # for comments)
db_name_for_keygrip() {
    [[ -f "$DEPLOY_KEYS_DB" ]] \
        && awk -v g="$1" '!/^#/ && $1==g { print $2; exit }' "$DEPLOY_KEYS_DB" \
        || true
}

db_keygrip_for_name() {
    [[ -f "$DEPLOY_KEYS_DB" ]] \
        && awk -v n="$1" '!/^#/ && $2==n { print $1; exit }' "$DEPLOY_KEYS_DB" \
        || true
}

# Accept either a deployment name or a raw keygrip; return the keygrip.
resolve_keygrip() {
    local input="$1"
    if [[ "$input" =~ ^[0-9A-Fa-f]{40}$ ]]; then
        echo "$input"
    else
        local grip
        grip=$(db_keygrip_for_name "$input")
        [[ -n "$grip" ]] || die "unknown deployment: $input"
        echo "$grip"
    fi
}

# Return the fingerprint of an auth subkey by keygrip.
fp_for_keygrip() {
    get_auth_subkeys | awk -v g="$1" '$2==g { print $1; exit }'
}

is_enabled() {
    [[ -f "$SSHCONTROL" ]] && grep -qE "^$1([[:space:]]|$)" "$SSHCONTROL"
}

fmt_date() {
    local ts="$1"
    [[ -z "$ts" || "$ts" == "0" ]] && echo "never" && return
    date -d "@$ts" "+%Y-%m-%d" 2>/dev/null || echo "$ts"
}

# ── Commands ───────────────────────────────────────────────────────────────────

cmd_add() {
    local name="${1:-}" expire="${2:-2y}"
    [[ -n "$name" ]] || die "usage: add <name> [<expire>]"

    # Guard against duplicate names
    local existing
    existing=$(db_keygrip_for_name "$name")
    [[ -z "$existing" ]] || die "deployment '$name' already has a subkey ($existing). Use 'list' to review."

    require_master_key

    info "Creating [A] subkey for deployment '$name' (expires: $expire)"
    gpg --quick-add-key "$MASTER_KEY_FP" ed25519 auth "$expire"

    # The newest auth subkey is the one just added (GPG outputs in creation order).
    local new_fp new_grip created expires
    read -r new_fp new_grip created expires < <(get_auth_subkeys | tail -1)
    [[ -n "$new_fp" && -n "$new_grip" ]] || die "could not locate the new subkey"

    info "Fingerprint : $new_fp"
    info "Keygrip     : $new_grip"

    # Record in DB
    touch "$DEPLOY_KEYS_DB"
    echo "$new_grip $name $(date +%Y-%m-%d)" >> "$DEPLOY_KEYS_DB"

    # Enable for SSH
    echo "$new_grip" >> "$SSHCONTROL"
    gpg-connect-agent reloadagent /bye >/dev/null 2>&1

    info "SSH public key for '$name' (add to authorized_keys on the target machine):"
    echo ""
    gpg --export-ssh-key "${new_fp}!"
    echo ""

    cat <<EOF
Next steps:
  1. Update the target machine's system config with the SSH public key above,
     then deploy it:
       guix deploy machines/$name/deployment.scm   # or equivalent

  2. Back up master key to IronKey NOW (includes the new subkey):
       gpg --armor --export-secret-keys $MASTER_KEY_FP \\
         > /media/ironkey/gpg/master-key.asc

  3. Re-strip the master key from this machine:
       gpg --export-secret-subkeys $MASTER_KEY_FP > /tmp/subkeys.gpg
       gpg --delete-secret-keys $MASTER_KEY_FP
       gpg --import /tmp/subkeys.gpg
       rm /tmp/subkeys.gpg
       gpg --list-secret-keys   # verify sec# and all ssb (no #)

  4. Commit the dotfiles changes (sshcontrol and deploy-keys.conf were updated):
       cd ~/.dotfiles && git add dotfiles/.gnupg/ && git commit

  WARNING: Do not strip before backing up to IronKey — the new subkey
           secret will be lost and cannot be recovered.
EOF
}

cmd_list() {
    echo "Deployment auth subkeys for ${MASTER_KEY_FP:0:16}..."
    echo ""

    local found=0
    while read -r fp grip created expires; do
        found=1
        local name status
        name=$(db_name_for_keygrip "$grip")
        [[ -n "$name" ]] || name="(unnamed)"

        if is_enabled "$grip"; then
            status="enabled"
        else
            status="disabled"
        fi

        printf "name      : %s\n"   "$name"
        printf "status    : %s\n"   "$status"
        printf "created   : %s\n"   "$(fmt_date "$created")"
        printf "expires   : %s\n"   "$(fmt_date "$expires")"
        printf "keygrip   : %s\n"   "$grip"
        printf "fingerprint: %s\n"  "$fp"
        printf "ssh pubkey : %s\n"  "$(gpg --export-ssh-key "${fp}!" 2>/dev/null || echo '(unavailable)')"
        echo ""
    done < <(get_auth_subkeys)

    [[ "$found" == 1 ]] || echo "(no auth subkeys found)"
}

cmd_pubkey() {
    local input="${1:-}"
    [[ -n "$input" ]] || die "usage: pubkey <name|keygrip>"

    local grip fp
    grip=$(resolve_keygrip "$input")
    fp=$(fp_for_keygrip "$grip")
    [[ -n "$fp" ]] || die "keygrip not found in GPG keyring: $grip"

    gpg --export-ssh-key "${fp}!"
}

cmd_enable() {
    local input="${1:-}"
    [[ -n "$input" ]] || die "usage: enable <name|keygrip>"

    local grip
    grip=$(resolve_keygrip "$input")

    if is_enabled "$grip"; then
        echo "already enabled: $grip"
        return
    fi

    # Remove any disabled (!grip) entry, then add bare grip
    touch "$SSHCONTROL"
    sed -i --follow-symlinks "/^!${grip}/d" "$SSHCONTROL"
    echo "$grip" >> "$SSHCONTROL"
    gpg-connect-agent reloadagent /bye >/dev/null 2>&1
    info "enabled: $grip"
}

cmd_disable() {
    local input="${1:-}"
    [[ -n "$input" ]] || die "usage: disable <name|keygrip>"

    local grip
    grip=$(resolve_keygrip "$input")

    touch "$SSHCONTROL"
    if grep -qE "^${grip}([[:space:]]|$)" "$SSHCONTROL"; then
        sed -i --follow-symlinks "s|^${grip}|!${grip}|" "$SSHCONTROL"
    elif ! grep -q "^!${grip}" "$SSHCONTROL"; then
        echo "!${grip}" >> "$SSHCONTROL"
    fi
    gpg-connect-agent reloadagent /bye >/dev/null 2>&1
    info "disabled: $grip"
}

# ── Usage ──────────────────────────────────────────────────────────────────────

usage() {
    cat <<'EOF'
Usage: manage-deploy-keys <command> [args]

Commands:
  add <name> [<expire>]    Create auth subkey for a deployment (requires master key)
  list                     List all deployment subkeys with status and SSH public keys
  pubkey <name|keygrip>    Print SSH public key for a deployment
  enable  <name|keygrip>   Enable subkey for SSH (add to sshcontrol)
  disable <name|keygrip>   Disable subkey for SSH

Environment:
  GPG_MASTER_KEY_FP        Override master key fingerprint

Examples:
  manage-deploy-keys add monk 2y
  manage-deploy-keys add alucard 2y
  manage-deploy-keys list
  manage-deploy-keys pubkey monk
  manage-deploy-keys disable monk
EOF
}

# ── Dispatch ───────────────────────────────────────────────────────────────────

case "${1:-}" in
    add)     cmd_add     "${2:-}" "${3:-}" ;;
    list)    cmd_list ;;
    pubkey)  cmd_pubkey  "${2:-}" ;;
    enable)  cmd_enable  "${2:-}" ;;
    disable) cmd_disable "${2:-}" ;;
    *)       usage; [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]] && exit 0 || exit 1 ;;
esac
