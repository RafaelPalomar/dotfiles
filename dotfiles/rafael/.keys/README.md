# Cryptographic Key Management

This directory contains the cryptographic infrastructure for entelequia.

## Documentation

**Canonical (start here):** `docs/source/secrets.rst` — the decision
tree (what goes where), the identity map, the SSH-pinning rule, and the
`keys` CLI. Who-holds-what: `docs/source/keys-inventory.md`. Why:
`docs/source/adr/`.

**Legacy GPG command reference / offline-master architecture:**
`docs/source/gpg.rst` (superseded as the entry point by `secrets.rst`).

Run `./scripts/build-docs.sh` to generate HTML/Info manuals.

## Directory Structure

- `gpg/master/` - Offline master key storage (on IronKey only)
- `gpg/public/` - Public key (safe to version control)
- `gpg/subkeys-backup/` - Encrypted subkey backups
- `ssh/` - SSH keys extracted from GPG
- `procedures/` - Quick reference procedures

## Key Information (Fill after generation)

- **Key ID:** `_______________`
- **Fingerprint:** `_______________`
- **Creation Date:** `_______________`
- **Signing Subkey Expiration:** `_______________`
- **Auth Subkey Expiration:** `_______________`

## Quick Check

```bash
# Verify master key is offline (should show sec#)
gpg --list-secret-keys

# Test SSH via GPG
ssh-add -L
```

## See Also

- Main docs: `docs/source/gpg.rst`
- GPG service: `entelequia/home/services/gpg.scm`
- Shell config: `entelequia/home/services/shell.scm`
