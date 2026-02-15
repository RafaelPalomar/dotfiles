# Cryptographic Key Management

This directory contains the cryptographic infrastructure for entelequia.

## Documentation

**Primary Documentation:** `/home/rafael/.dotfiles/docs/source/gpg.rst`

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
