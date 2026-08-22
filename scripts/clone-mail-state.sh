#!/usr/bin/env bash
# clone-mail-state.sh — replicate this machine's mail + secret state onto
# another of rafael's boxes, so the target needs NO re-authentication.
#
# The *declarative* half of the mail stack (isync, msmtp, notmuch, mu,
# mutt_oauth2, ~/.mbsyncrc, ~/.notmuch-config{,-agent}, ~/.msmtprc and the
# mail-* / sync-mail helper scripts) already ships to every machine tagged
# `home-role 'work` — see entelequia/home/profiles/role.scm.  `guix home
# reconfigure` installs all of it, and this script verifies it is there
# before touching anything.
#
# What reconfigure CANNOT carry is the mutable state:
#
#   ~/.gnupg             the keypair everything below is encrypted to
#   ~/.password-store    incl. email/{ntnu.no,uio.no}.gpg — the OAuth2
#                        refresh tokens.  Copying these is precisely what
#                        makes the Azure app-registration + browser
#                        authorize dance (scripts/bootstrap-email-oauth2.sh,
#                        docs/EMAIL-SETUP.md) unnecessary on the target.
#   ~/.local/share/mail  the maildir + mbsync sync state
#   notmuch tags         carried via `notmuch dump` / `restore`, NOT by
#                        copying the Xapian DB (version-coupled, and unsafe
#                        to copy while live)
#
# ~/.local/share/mail-agent/ is deliberately NOT copied: it is a derived
# symlink tree, rebuilt on the target by `sync-mail-agent-index` so the
# folder allowlist is re-applied there rather than inherited.
#
# Transport is tar-over-ssh: desktop hosts carry no rsync (it is only in
# system/layers/server-base.scm), and tar wins anyway on a bulk first copy
# of tens of thousands of maildir files.  Re-running recopies rather than
# syncing incrementally — fine for a one-shot bootstrap.
#
# THIS SCRIPT MOVES LIVE CREDENTIALS AND A GPG SECRET KEY.  Run it
# yourself, from the source machine, over a trusted link (tailnet, campus,
# LAN).  Claude Code cannot run it: ~/.password-store and
# ~/.local/share/mail are on its deny-list, and secret-key material
# belongs on a human keystroke — same rule as mail send (`C-c C-c`),
# ntnu-vpn-up and lgogdownloader --login.
#
# Usage:
#   scripts/clone-mail-state.sh <host>              full clone
#   scripts/clone-mail-state.sh <host> --no-mail    skip the maildir; let
#                                                   sync-mail refetch it
#   scripts/clone-mail-state.sh <host> --no-gpg     target already holds
#                                                   the decryption subkey
#   scripts/clone-mail-state.sh <host> --dry-run    print, do nothing
#
# <host> is anything ssh(1) resolves — an ~/.ssh/config alias, or
# 'rafael@10.54.212.26 -p 2222' style needs an alias instead.  einstein is
# reachable on the campus subnet (10.54.212.26:2222), not the personal
# tailnet; see entelequia/deploy/einstein.scm.

set -euo pipefail

MAIL_ROOT="$HOME/.local/share/mail"
PASS_STORE="$HOME/.password-store"

COPY_MAIL=1
COPY_GPG=1
DRY_RUN=0
TARGET=""

die()  { printf '\033[31merror:\033[0m %s\n' "$*" >&2; exit 1; }
step() { printf '\n\033[1;34m==>\033[0m \033[1m%s\033[0m\n' "$*"; }
note() { printf '    %s\n' "$*"; }
ok()   { printf '    \033[32mok\033[0m   %s\n' "$*"; }
warn() { printf '    \033[33mwarn\033[0m %s\n' "$*"; }
run()  {
  if [ "$DRY_RUN" -eq 1 ]; then printf '    \033[2m[dry-run] %s\033[0m\n' "$*"; return 0; fi
  eval "$@"
}

while [ $# -gt 0 ]; do
  case "$1" in
    --no-mail) COPY_MAIL=0 ;;
    --no-gpg)  COPY_GPG=0 ;;
    --dry-run) DRY_RUN=1 ;;
    -h|--help) sed -n '/^# Usage:/,$p' "$0" | sed -n '/^#/s/^# \{0,1\}//p'; exit 0 ;;
    -*)        die "unknown flag: $1" ;;
    *)         [ -z "$TARGET" ] || die "only one target host"; TARGET="$1" ;;
  esac
  shift
done

[ -n "$TARGET" ] || die "no target host.  usage: $0 <host> [--no-mail] [--no-gpg] [--dry-run]"
[ "$TARGET" != "$(hostname)" ] || die "target is this machine"

SSH_OPTS="-o ConnectTimeout=15"
SSH="ssh $SSH_OPTS $TARGET"
# ssh(1) is `ssh [options] host [command]` -- options must precede the
# host, so a tty-allocating variant needs its own spelling rather than
# "$SSH -t", which would pass -t as argv[0] of the remote command.
SSH_TTY_="ssh $SSH_OPTS -t $TARGET"
# NB: ~/.local/bin holds sync-mail / sync-mail-agent-index / notmuch-agent but
# is only prepended to PATH by the home env's login profile
# (desktop-suite.scm:152), which a non-interactive `ssh host cmd` does not
# source.  Those helpers are therefore invoked by absolute path below, via the
# resolved $REMOTE_HOME -- never by bare name.

# --- 0. Preflight -----------------------------------------------------
step "Preflight"

for b in gpg tar notmuch; do
  command -v "$b" >/dev/null || die "missing locally: $b"
done
[ -d "$PASS_STORE" ] || die "no password store at $PASS_STORE"

if [ "$COPY_MAIL" -eq 1 ]; then
  [ -d "$MAIL_ROOT" ] || die "no maildir at $MAIL_ROOT (pass --no-mail to skip)"
  if pgrep -x mbsync >/dev/null; then
    die "mbsync is running — wait for sync-mail to finish, else the copy is torn"
  fi
fi

ssh $SSH_OPTS -o BatchMode=yes "$TARGET" true 2>/dev/null \
  || die "cannot ssh to $TARGET as $(whoami).

If the target's sshd only trusts the *root* deploy key so far, add your
login key to its #:ssh-authorized-keys in entelequia/system/machines/
and deploy:

    guix time-machine -C ~/.dotfiles/channels-lock.scm -- \\
      deploy -L ~/.dotfiles entelequia/deploy/<host>.scm"

REMOTE_HOME=$($SSH 'echo $HOME')
[ -n "$REMOTE_HOME" ] || die "could not resolve \$HOME on $TARGET"
ok "ssh $TARGET  (HOME=$REMOTE_HOME)"

# The target must already have the work-role home env deployed, else none of
# the mail binaries, configs or helper scripts exist there.
MISSING=$($SSH 'for b in mbsync msmtp notmuch mutt_oauth2.py mu gpg tar; do
                  command -v "$b" >/dev/null 2>&1 || printf "%s " "$b"; done
                for f in .mbsyncrc .notmuch-config .notmuch-config-agent .msmtprc; do
                  [ -e "$HOME/$f" ] || printf "~/%s " "$f"; done
                for s in sync-mail sync-mail-agent-index notmuch-agent; do
                  [ -x "$HOME/.local/bin/$s" ] || printf "~/.local/bin/%s " "$s"; done') || true
if [ -n "${MISSING// /}" ]; then
  die "$TARGET is missing: $MISSING

Deploy the home environment there first — it already declares the whole
mail stack via home-role-packages 'work:

    ssh $TARGET
    git -C ~/.dotfiles pull && home-reconfigure"
fi
ok "target has the mail stack, configs and helper scripts"

[ "$($SSH 'cat "$HOME/.config/entelequia/role" 2>/dev/null')" = work ] \
  || warn "$TARGET is not tagged home-role 'work — mail state there is off-plan"

# Non-fatal: sops/rafael.yaml is only needed for the auth-email-* aliases,
# i.e. re-authorizing an account from scratch.  The tokens copied below make
# that unnecessary in the normal case, but flag it so a later token failure
# on the target is not a surprise.
$SSH '[ -r "$HOME/.dotfiles/sops/rafael.yaml" ]' 2>/dev/null \
  || warn "$TARGET has no readable ~/.dotfiles/sops/rafael.yaml — the
         auth-email-ntnu / auth-email-uio fallbacks will not work there
         until its ~/.dotfiles checkout is brought up to date
         (git -C ~/.dotfiles pull).  Not needed if the copied tokens work."

# --- 1. GPG keys ------------------------------------------------------
# Everything below is encrypted to these.  The primary of 0x70350DAD is a
# stub (sec#) here — offline-primary setup — so only subkeys travel.  The
# cv25519 [E] subkey is the one pass and mutt_oauth2.py decrypt with.
# The import is additive and idempotent: keys already on the target are
# left alone, missing subkeys are added.
if [ "$COPY_GPG" -eq 1 ]; then
  step "GPG keys -> $TARGET"
  note "secret keys on this machine:"
  gpg --list-secret-keys --keyid-format=long 2>/dev/null \
    | grep -E '^(sec|ssb)' | sed 's/^/      /'
  note ""
  note "already on $TARGET:"
  $SSH 'gpg --list-secret-keys --keyid-format=long 2>/dev/null' \
    | grep -E '^(sec|ssb)' | sed 's/^/      /' || note "      (none)"

  if [ "$DRY_RUN" -eq 0 ]; then
    printf '\n    Copy the secret keys above to %s? [y/N] ' "$TARGET"
    read -r reply
    case "$reply" in [Yy]*) ;; *) die "aborted" ;; esac
  fi

  run "gpg --export --armor | $SSH 'gpg --import' 2>&1 | sed 's/^/      /'"
  run "gpg --export-secret-keys --armor | $SSH 'gpg --import --batch' 2>&1 | sed 's/^/      /'"
  run "gpg --export-ownertrust | $SSH 'gpg --import-ownertrust' 2>&1 | sed 's/^/      /'"
  ok "keys imported — gpg-agent on $TARGET prompts for the passphrase on first use"
else
  step "GPG keys: SKIPPED (--no-gpg)"
fi

# --- 2. Password store ------------------------------------------------
# Holds email/ntnu.no.gpg and email/uio.no.gpg — the OAuth2 refresh tokens
# that .mbsyncrc's PassCmd and .msmtprc's passwordeval hand to
# mutt_oauth2.py.  Copying them is what skips re-authentication.
step "Password store -> $TARGET"
run "tar -C '$HOME' -cf - .password-store \
     | $SSH 'tar -C \"$REMOTE_HOME\" -xf - --overwrite'"
run "$SSH 'chmod -R go-rwx \"$REMOTE_HOME/.password-store\"'"
ok "$PASS_STORE -> $TARGET:$REMOTE_HOME/.password-store"
if [ "$DRY_RUN" -eq 0 ]; then
  $SSH '[ -f "$HOME/.password-store/email/ntnu.no.gpg" ] \
        && [ -f "$HOME/.password-store/email/uio.no.gpg" ]' \
    && ok "both OAuth2 token files landed" \
    || warn "OAuth2 token files not found on the target after copy"
fi

# --- 3. Maildir + notmuch tags ---------------------------------------
if [ "$COPY_MAIL" -eq 1 ]; then
  step "Maildir -> $TARGET  (the slow step)"
  note "excluding .notmuch/ — the Xapian DB is rebuilt below; tags follow separately"
  # No -H: that is GNU tar's --format= (it would swallow the --exclude).
  # Hard links between folders, which mbsync and mu4e create, are preserved
  # by GNU tar by default.
  run "tar -C '$HOME/.local/share' -cf - --exclude='mail/.notmuch' mail \
       | $SSH 'mkdir -p \"$REMOTE_HOME/.local/share\" \
                && tar -C \"$REMOTE_HOME/.local/share\" -xf - --overwrite'"
  ok "maildir copied"

  step "notmuch tags -> $TARGET"
  # `notmuch dump` is keyed by Message-ID, so it is machine-independent.
  run "$SSH 'mkdir -p \"$REMOTE_HOME/.cache\"'"
  run "notmuch dump --format=batch-tag \
       | $SSH 'cat > \"$REMOTE_HOME/.cache/notmuch-tags.dump\"'"
  note "indexing on $TARGET — first pass over the whole maildir, expect minutes"
  run "$SSH 'notmuch new --quiet \
             && notmuch restore --format=batch-tag \
                  --input=\"$REMOTE_HOME/.cache/notmuch-tags.dump\" \
             && rm -f \"$REMOTE_HOME/.cache/notmuch-tags.dump\"'"
  ok "human notmuch DB built with tags restored"

  step "Agent mail view + mu index on $TARGET"
  # The agent's filtered view is a symlink tree: rebuild it there so the
  # ALLOW_FOLDERS allowlist is freshly applied, never copied.
  run "$SSH '$REMOTE_HOME/.local/bin/sync-mail-agent-index'"
  ok "~/.local/share/mail-agent/ rebuilt from the allowlist"
  run "$SSH 'mu init --quiet \
               --maildir=\"$REMOTE_HOME/.local/share/mail\" \
               --my-address=rafael.palomar@ous-research.no \
               --my-address=rafael.palomar@ntnu.no \
             && mu index --quiet'"
  ok "mu4e index built"
else
  step "Maildir: SKIPPED (--no-mail)"
  note "run 'sync-mail' on $TARGET to fetch from O365 — auth already works,"
  note "and it rebuilds both notmuch DBs plus the agent view on its own."
fi

# --- 4. Verify the tokens actually work ------------------------------
step "Verifying OAuth2 tokens on $TARGET"
if [ "$DRY_RUN" -eq 1 ]; then
  note "[dry-run] would run mutt_oauth2.py --test for ntnu.no and uio.no"
else
  note "each --test hits Microsoft and refreshes if needed; gpg-agent on"
  note "$TARGET will ask for your passphrase the first time"
  FAILED=""
  for acct in ntnu.no uio.no; do
    if $SSH_TTY_ "mutt_oauth2.py \"\$HOME/.password-store/email/$acct.gpg\" --test"; then
      ok "$acct"
    else
      FAILED="$FAILED $acct"
      warn "$acct token test failed"
    fi
  done
fi

step "Done"
cat <<EOF
    On $TARGET:
      sync-mail          fetch + index both accounts (human-only step)
      M-x mu4e           over the copied maildir
      notmuch-agent ...  the agent's filtered view

    If a token test failed, authorize that one account once on $TARGET:
      auth-email-ntnu    /  auth-email-uio
    (bash aliases; they read OAUTH_CLIENT_ID/SECRET out of
     ~/.dotfiles/sops/rafael.yaml via the GPG key just imported.)

    Both machines now sync the same two O365 accounts.  mbsync supports
    that — each keeps its own local state and reconciles against the
    server — but run sync-mail on one box at a time to avoid racing on
    IMAP flags.
EOF
