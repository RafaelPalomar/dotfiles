# Channel authentication for self-owned channels (deferred)

Status: **deferred 2026-07-04** pending an IronKey session.
Decision: mint a dedicated channel-signing subkey under the offline
master (`70350DAD507FA72F`) rather than reusing the work key
(`9440CF71CAEA2D0B`) or creating a standalone key.

## Why

10 of 13 channels in `channels.scm` carry no `channel-introduction`,
so `guix pull -C channels.scm` fetches unauthenticated HEADs.  The
lock file's commit pins protect `time-machine` runs, but a compromised
forge account on `guix-xlibre` (the X server) or `tailscale` (the VPN
daemon) is root-level code execution fleet-wide on the next lock bump.

Interim mitigations (active now): commit-pinned `channels-lock.scm` is
the only executed path; forge accounts have 2FA.

## Procedure (one IronKey session + per-channel setup)

1. **Mint the subkey** (IronKey mounted, offline master imported):

   ```bash
   gpg --homedir /path/to/ironkey/gnupg --quick-add-key \
       "<master-fpr>" ed25519 sign 2y
   # export ONLY the new subkey's secret to the online keyring:
   gpg --homedir ... --export-secret-subkeys <subkey-id>! | gpg --import
   ```

2. **Configure git signing** (global, since agents push too):

   ```bash
   git config --global user.signingkey <subkey-id>!
   git config --global commit.gpgsign true
   ```

   Note: from this point **every** commit to an authenticated channel
   must be signed — including agent-driven pushes.  gpg-agent must be
   able to sign non-interactively (the subkey should have no
   passphrase or a cached one), and each channel repo needs a
   pre-receive/CI guard so an unsigned commit can never land (it would
   wedge `guix pull` for every consumer).

3. **Per channel** (pilot: `guix-xlibre`, then `tailscale`,
   `alpha-agent`, `archimedes-agent`, then the OUH-MESHLab org
   channels):

   ```bash
   # a. keyring branch with the signing public key:
   git checkout --orphan keyring
   gpg --export <subkey-id> > rafael.key   # binary, .key extension
   git add rafael.key && git commit -m "Add signing key" && git push origin keyring

   # b. back on master: authorizations file
   git checkout master
   cat > .guix-authorizations <<'EOF'
   (authorizations
    (version 0)
    (("<subkey-fingerprint>"
      (name "rafael"))))
   EOF
   git add .guix-authorizations
   git commit -S -m "Authenticate channel commits"   # FIRST SIGNED COMMIT
   git push
   ```

4. **Introduce the channel** in `channels.scm` (and regen the lock):

   ```scheme
   (channel
     (name 'guix-xlibre)
     (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
     (branch "master")
     (introduction
      (make-channel-introduction
       "<commit hash of the FIRST SIGNED commit from step 3b>"
       (openpgp-fingerprint "<subkey-fingerprint>"))))
   ```

5. **Verify** before committing the introduction:

   ```bash
   guix pull -C channels.scm --dry-run   # must authenticate the channel
   ./scripts/update-lock.sh
   ```

## Gotchas

- The introduction commit must be the first signed one; earlier
  history stays unauthenticated (fine — the introduction anchors trust
  from that point forward).
- `.guix-authorizations` changes must themselves be signed by a key
  authorized in the *parent* commit — key rotation is append-then-swap
  across two commits, never a single replace.
- Guix reads the keyring from the `keyring` branch by default
  (`.guix-channel` can override with `(keyring-reference "...")`).
- If an unsigned commit ever lands, consumers' `guix pull` fails hard;
  fixing requires history rewrite or a new introduction.  Hence the
  push guard in step 2.
