# Pre-Key-Generation Checklist

**Date:** 2026-02-15

Before generating GPG keys, verify these preparations are complete:

## Infrastructure Setup

- [x] Shell configured with GPG_TTY and SSH_AUTH_SOCK
- [x] GPG agent service with SSH support enabled
- [x] Keys directory structure created (~/.keys)
- [ ] Git configuration template created (pending)
- [ ] SSH config template created (pending)
- [ ] Helper scripts created (pending)

## IronKey Preparation

- [ ] IronKey formatted and encrypted
- [ ] IronKey directory structure created:
  - `/media/ironkey/gpg/`
  - `/media/ironkey/ssh/`
  - `/media/ironkey/procedures/`
- [ ] IronKey tested (mount/unmount)
- [ ] Backup passphrase chosen (6+ words, diceware)

## Physical Security

- [ ] Safe/lockbox available for revocation certificate
- [ ] Secondary backup location identified (optional)
- [ ] Printed storage plan documented

## System State

- [ ] Guix system reconfigured with updated shell service
- [ ] GPG agent restarted: `gpgconf --kill gpg-agent`
- [ ] Environment variables verified: `echo $SSH_AUTH_SOCK`

## Knowledge

- [ ] Read full strategy document
- [ ] Understand master key vs subkeys
- [ ] Understand key rotation procedures
- [ ] Know where revocation cert will be stored

## Next Steps

After checklist complete:

1. Review `/home/rafael/.dotfiles/docs/source/gpg.rst`
2. Connect IronKey
3. Begin Phase 1: Key Generation (see strategy document)
