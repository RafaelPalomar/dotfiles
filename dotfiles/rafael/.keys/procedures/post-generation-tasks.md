# Post-Key-Generation Tasks

**Complete after generating keys**

## Key Information (Record Here)

```
Key ID: _______________
Fingerprint: _______________
Creation Date: _______________
Master Key Location: /media/ironkey/gpg/master-key.asc.gpg
Revocation Cert Location: _______________
```

## Immediate Tasks

- [ ] Update Git config with signing key ID
- [ ] Verify master key is offline on einstein: `gpg -K` shows `sec#`
- [ ] Verify master key is offline on curie: `gpg -K` shows `sec#`
- [ ] Export SSH public key: `gpg --export-ssh-key KEYID`
- [ ] Add SSH key to GitHub: https://github.com/settings/keys
- [ ] Add SSH key to einstein/curie for mutual access
- [ ] Test git commit signing: `git commit --allow-empty -S -m "Test"`
- [ ] Test SSH authentication: `ssh -T git@github.com`

## Documentation Updates

- [ ] Fill in key information in `~/.keys/README.md`
- [ ] Update `docs/source/gpg.rst` with actual key ID
- [ ] Rebuild docs: `./scripts/build-docs.sh`

## Backup Verification

- [ ] IronKey master key backup verified
- [ ] Revocation certificate printed and stored
- [ ] Subkeys backed up to `~/.keys/gpg/subkeys-backup/`
- [ ] Calendar reminder: Subkey expiration - 1 month

## Remote Systems (if applicable)

- [ ] Add SSH public key to remote servers
- [ ] Test guix deploy with GPG SSH authentication
- [ ] Distribute public key to managed systems

## See Main Documentation

`/home/rafael/.dotfiles/docs/source/gpg.rst`
