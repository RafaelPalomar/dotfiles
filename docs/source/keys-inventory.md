<!-- last updated 2026-05-30 -->
# Personal key inventory

Source of truth for every GnuPG key the **personal** entelequia scope
depends on. Mirrors the discipline of
`~/src/ivs-infrastructure/Docs/keys-inventory.md` (its ADR-0006), lifted
to the personal scope per this repo's ADR-0001.

**Rules:**

- Every key that signs, authenticates, or decrypts a personal secret
  appears here.
- Updates land in the *same commit* as the key change — never "later".
- `keys audit` (Phase 1) reconciles this file against live keyrings; it
  must exit 0 on `main`.
- Fingerprints are recorded with spaces stripped, uppercase, full
  40 hex chars.
- The rotation log is append-only.

## Master key

The long-lived, identity-bearing certification key. **Air-gapped** on
the IronKey; only a stub (`sec#`) lives on daily-use machines. Used only
for certification (adding/rotating subkeys) via `keys ceremony`.

| UID | Fingerprint | Algo | Created | Expires | Lives on | Escrow | Purpose |
|---|---|---|---|---|---|---|---|
| Rafael Palomar &lt;rafael@palomar.no&gt; | `6513C7248D7BECE2EC1BD34B70350DAD507FA72F` | RSA-4096 `[C]` | 2026-02-15 | never | IronKey (offline); `sec#` stub on einstein/curie | IronKey (triple-encrypted) + printed revocation cert in safe. **TODO:** add `paperkey` printout. | Certification root for all personal subkeys below |

## Daily subkeys

Held in `~/.gnupg` on einstein/curie, unlocked at login via
`pam-gnupg`. Regenerable from the master via `keys ceremony`.

| Capability | Fingerprint | Algo | Created | Expires | Purpose |
|---|---|---|---|---|---|
| `[S]` sign | `65351744FC426D6DE571B84A35530619B58AD70A` | ed25519 | 2026-02-15 | 2028-02-15 | git commit/tag signing, document/package signatures |
| `[E]` encrypt | `C14D2DEB723B6540CEE3DE28D440F671F4ACDFAE` | cv25519 | 2026-02-15 | 2028-02-15 | encrypted mail/files; `pass` + dotfiles SOPS recipient |
| `[SA]` sign+auth | `53A2D043E0DC9249DF75ABB665D896E00C101DDF` | ed25519 | 2026-02-15 | 2028-02-15 | primary SSH auth (GitHub, own machines) |

## Deploy keys

Per-deployment `[A]` auth subkeys of the master, each used as one
machine's SSH deploy key. Managed by `keys deploy` (today:
`manage-deploy-keys.sh`); keygrips tracked in
`dotfiles/rafael/.gnupg/deploy-keys.conf` + `sshcontrol`. Each must be **pinned
per-host** in the SSH client config (ADR-0003) so the agent never
exceeds `MaxAuthTries`.

| Deployment | Fingerprint | Algo | Created | Expires |
|---|---|---|---|---|
| monk | `3CC562C283D551CB704CEC99A0CCD95447F4E1F7` | ed25519 | 2026-03-09 | 2028-03-08 |
| lovelace | `8FCB604EA6D7637FB36A5C7F908C95BA96CFC574` | ed25519 | 2026-03-19 | 2028-03-18 |
| edison | `A08C8C2F50C18800EAC68692AFADD4596B62CDA0` | ed25519 | 2026-03-31 | 2028-03-30 |
| hopper | `8CD379CC5C542D09F0DBC9B63CB146150266C7CE` | ed25519 | 2026-04-26 | 2028-04-25 |
| hamming | `A78222CD3E402A13615BE31695A6C102BB5E75C1` | ed25519 | 2026-05-27 | 2028-05-26 |
| baroja | `B618FB7C059C8D10EFAD6D663B523DC7C2B1C020` | ed25519 | 2026-05-27 | 2028-05-26 |

## Host SOPS keys

Per-machine SOPS keys: each `guix deploy`'d machine has a dedicated key
that decrypts its `dotfiles/sops/<machine>.yaml` at boot (the personal
master is the secondary recipient so the operator can edit from
curie/einstein). The private half lives on the machine
(`/var/lib/sops`); the **public** half is imported on the operator
workstation so `sops` can encrypt to it. Recipients are declared in
`.sops.yaml`.

| UID | Fingerprint | Algo | Created | Expires | Lives on | Escrow | Purpose |
|---|---|---|---|---|---|---|---|
| Lovelace SOPS &lt;lovelace-sops@palomar.no&gt; | `0E4534607A2FA8D112176DCEDDAA34F42A158809` | ed25519 + cv25519 | 2026-03-20 | never | lovelace:/var/lib/sops (public on curie) | (not escrowed — regenerate + `sops updatekeys` on host loss) | Recipient for `sops/lovelace.yaml` |
| Edison SOPS &lt;edison-sops@palomar.no&gt; | `1902E1477A9D19FD73BBF7F47050A7747D28D4A9` | ed25519 + cv25519 | 2026-04-01 | never | edison:/var/lib/sops (public on curie) | (not escrowed — regenerate + `sops updatekeys` on host loss) | Recipient for `sops/edison.yaml` |
| Alucard SOPS &lt;alucard-sops@entelequia&gt; | `B607E174ADDBEA492F8D0E6162032497FA92CF96` | ed25519 + cv25519 | 2026-06-17 | 2028-06-16 | alucard:/root/.gnupg (public on curie) | (not escrowed — regenerate on host loss) | Recipient for `sops/alucard.yaml` (Archimedes tutor key, leandro) |
| Curie SOPS &lt;curie-sops@entelequia&gt; | `F85D92CAFE560BA70C32E776D4AAAB1C40D8029D` | ed25519 + cv25519 | 2026-06-17 | never | curie:/root/.gnupg | (not escrowed — regenerate on host loss) | Recipient for `sops/curie.yaml` (alpha personal-agent key, rafael) |
| Hopper SOPS &lt;hopper-sops@entelequia&gt; | `61777DDACBDCE29C340DDF1582D249E14856D496` | ed25519 + cv25519 | 2026-06-18 | never | hopper:/root/.gnupg (public on curie) | (not escrowed — regenerate on host loss) | Recipient for `sops/hopper.yaml` (Archimedes tutor key, adrian) |
| Baroja SOPS &lt;baroja-sops@entelequia&gt; | `7BFF3457442479BAD396C122AE6968E8FC6C9607` | ed25519 + cv25519 | 2026-08-01 | never | baroja:/root/.gnupg (public on curie) | (not escrowed — regenerate on host loss) | Recipient for `sops/baroja.yaml` (alpha personal-agent key, rafael) |

## Adjacent (informational)

Keys reachable from the operator workstation that are **not** part of
the personal scope. Listed so `keys audit` does not flag them and so a
reader does not confuse them with personal keys.

| UID | Fingerprint | Used for | Note |
|---|---|---|---|
| Rafael Palomar &lt;rafpal@ous-hf.no&gt; (primary); Rafael IVS SOPS &lt;ivs-sops@palomar.no&gt; (secondary) | `8EADF28F4F8DC23942345E9A9440CF71CAEA2D0B` | `~/src/ivs-infrastructure` SOPS operator recipient | Research-side IVS operator key, **separate** from the personal master. **UID reconciled 2026-05-30** to `rafpal@ous-hf.no` (now primary; old UID retained). Fingerprint unchanged → SOPS recipients unaffected. **Follow-up (IVS repo):** import `/tmp/ivs-operator-pub.asc` to hosts/keyservers + update IVS `keys-inventory.md` UID column. |

## Outstanding items

- [ ] **Personal master `paperkey`** printed and added to the safe.
      *Tooling ready:* `keys paperkey --identity personal --yes`
      (IronKey/tmpfs session) → print → `shred` the file. Operator
      action pending (needs IronKey + master passphrase).
- [ ] **IVS operator key escrow** of `8EADF28F…`. *Tooling ready:*
      `keys escrow ivs --to <path>.gpg --yes` (encrypts to the personal
      master, auto-verifies). Place output on the IronKey or in the IVS
      repo — **not** this personal repo. Operator action pending (needs
      the IVS key passphrase at the pinentry prompt).
- [x] **IVS operator UID reconciliation** to `Rafael Palomar
      <rafpal@ous-hf.no>` — done 2026-05-30 (`--quick-add-uid` +
      `--quick-set-primary-uid`; old UID retained as secondary; FP
      unchanged). **IVS-repo follow-up still open:** import
      `/tmp/ivs-operator-pub.asc` to hosts/keyservers + update the IVS
      repo's `keys-inventory.md` UID column.
- [x] **`keys audit`** implemented (Phase 1) and wired as a dotfiles
      pre-commit hook + static CI gate (`.githooks/pre-commit`,
      `.github/workflows/keys-audit.yml`).

## Rotation log

Append-only. One row per create / rotate / revoke, newest first.

| Date | Key | Action | Reason |
|---|---|---|---|
| 2026-08-01 | Baroja SOPS `7BFF3457…` | created | Per-machine SOPS key for baroja (`sops/baroja.yaml`); alpha personal-agent key for rafael |
| 2026-06-18 | Hopper SOPS `61777DDA…` | created | Per-machine SOPS key for hopper (`sops/hopper.yaml`); Archimedes tutor key for adrian |
| 2026-05-30 | IVS operator `8EADF28F…` | uid-reconciled | Added + set primary `Rafael Palomar <rafpal@ous-hf.no>`; old `ivs-sops@palomar.no` retained as secondary. FP unchanged → SOPS unaffected (ADR-0001) |
| 2026-05-30 | (inventory) | created | Phase 0 of the key-management consolidation: personal inventory established as source of truth (ADR-0001) |
| 2026-05-27 | hamming, baroja deploy subkeys | created | Deploy auth subkeys for hamming/baroja hosts |
| 2026-04-26 | hopper deploy subkey | created | Deploy auth subkey for hopper |
| 2026-04-01 | Edison SOPS | created | Per-machine SOPS key for edison (`sops/edison.yaml`); back-documented 2026-05-30 |
| 2026-03-31 | edison deploy subkey | created | Deploy auth subkey for edison |
| 2026-03-20 | Lovelace SOPS | created | Per-machine SOPS key for lovelace (`sops/lovelace.yaml`); back-documented 2026-05-30 |
| 2026-03-19 | lovelace deploy subkey | created | Deploy auth subkey for lovelace |
| 2026-03-09 | monk deploy subkey | created | Deploy auth subkey for monk |
| 2026-02-15 | personal master + `[S][E][SA]` subkeys | created | Initial offline-master GnuPG infrastructure (SETUP-COMPLETE) |
