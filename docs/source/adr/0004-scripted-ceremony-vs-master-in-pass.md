# 0004. Scripted IronKey ceremony, not master-in-pass

- **Status:** Accepted
- **Date:** 2026-05-30
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

Creating or rotating a GnuPG subkey requires the offline-master
ceremony: transfer the master from the IronKey, generate the subkey,
strip the secret master, copy the stripped working keyring back. It is
manual and fiddly (mount, set `GNUPGHOME`, the right `gpg`
incantations, `--export-secret-subkeys` strip, copy back, verify), which
recurs every time a deploy key is added — and that friction tempted a
"store the master in `pass` and script generation" replacement.

That temptation is a trap. `pass` is a GnuPG-encrypted file tree on
curie's disk, synced via git. Storing the master there means:

1. The encrypted master blob now lives on curie's disk (and in git
   history wherever `pass` syncs). **The air-gap is gone** — the bytes
   are present, merely encrypted.
2. Security collapses onto whatever key encrypts that `pass` entry —
   which cannot be the master itself (chicken-and-egg), so it must be a
   separate vault key that needs the *same* protection the master had
   and *must* live on curie to be usable without the IronKey (the whole
   point of the change).
3. A curie compromise during any decrypt then equals a master
   compromise — the exact attack the IronKey model defeats.

The pain being solved is the **manual fumbling**, not the air-gap. The
air-gap *is* the value: the master private bytes are never present on an
online daily-use machine, so curie can be fully compromised and the
attacker still cannot reach the identity-bearing master.

## Decision

**Keep the master air-gapped on the IronKey; script the ceremony.**
Build `keys ceremony` to automate the fumbling while preserving the
air-gap end-to-end:

```text
keys ceremony add-subkey --identity personal --type auth --expire 2y
```

which: mounts the IronKey → spins a **tmpfs** `GNUPGHOME` → imports the
master from the token → generates/rotates the subkey → exports the
stripped working keyring back to curie → wipes the tmpfs → unmounts. A
hard rail **refuses** if the master would be written to persistent disk.
Each ceremony appends an audit row (date, identity, subkey fingerprint,
purpose) to `docs/source/keys-inventory.md`.

This retires the eight loose, untracked `~/.local/bin/gpg-*.sh` scripts,
folding them into one vetted, version-controlled command.

## Alternatives considered

### Alternative A — store master(s) in `pass`, script generation

Eliminates the physical IronKey transfer. Rejected: relocates the trust
root from "hardware token in a drawer" to "a key on the online laptop"
(see Context) — a downgrade for a long-lived identity key. The day curie
is popped, you trade a CVE for an identity compromise.

### Alternative B — master-in-pass *only for the lower-value identity*

The IVS operator key (research-side, 2-year subkeys, scoped,
regenerable) has a bounded blast radius, so master-in-pass is *less*
catastrophic for it. Rejected for now: only worth it if the ceremony is
frequent for IVS, which at 2-year subkey expiry it is not — the scripted
ceremony already removes the friction without the security cost.

### Alternative C — air-gapped VM / Tails boot as ceremony host

Use a disposable airgapped boot instead of the IronKey-on-curie dance.
**Deferred, not rejected** — a stronger ceremony host than tmpfs-on-curie
if the IronKey workflow proves insufficient. Recorded as a future option
for `keys ceremony --host`.

## Consequences

**Easier:** subkey add/rotate becomes one command; the air-gap is
preserved automatically (the rail enforces it); the loose `gpg-*.sh`
scripts and their drift disappear; ceremonies are audit-logged.

**Harder / follow-up:**

- `keys ceremony` must correctly handle IronKey mount/unmount detection,
  tmpfs `GNUPGHOME` lifecycle, and verification — this is the most
  security-sensitive part of the `keys` CLI and warrants careful review
  (Phase 1).
- Backup/escrow posture is adjacent: `paperkey` of the master, second
  IronKey, where the stripped working keyring is allowed to live — see
  the inventory's *Outstanding items*.
- **Backup integrity must be checksummed by *identity*, not bytes.**
  GnuPG re-applies S2K protection with a fresh random salt on every
  secret-key export, so `sha256(--export-secret-keys)` is not
  reproducible. `keys ceremony` therefore writes two checksums beside
  the IronKey master backup — a per-write *storage* checksum
  (`master-key.asc.sha256`, for bit-rot of that file) and a
  *reproducible identity manifest* checksum (`master-key.manifest.sha256`
  = sorted fingerprints + keygrips + caps + dates) — and `keys backup
  verify` checks both.

## Conformance

- **Asserts:** after a `keys ceremony` run, `gpg --list-secret-keys`
  shows the master as `sec#` (secret absent) on curie, the new subkey is
  present (`ssb`), and no plaintext master export remains on persistent
  disk. A new inventory rotation-log row exists for the operation.
- **Backup-integrity test:** `keys backup verify --file <master backup>`
  exits 0 — storage checksum matches, the secret primary is present in a
  tmpfs import, and the identity manifest matches `master-key.manifest.sha256`.

## References

- Origin: `/tmp/key-ceremony-vs-master-in-pass.md` (brainstorm,
  2026-05-30) — this ADR promotes it.
- `dotfiles/.local/bin/manage-deploy-keys.sh` — current per-deploy
  ceremony, to be subsumed by `keys ceremony` / `keys deploy`.
- `~/src/ivs-infrastructure/Docs/adr/0006` — key-inventory + audit
  discipline the audit-log row ties into.
- PKS: a permanent note promoting the air-gap trade-off reasoning
  (companion to this ADR).
