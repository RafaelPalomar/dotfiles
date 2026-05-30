# 0001. Trust roots & identities

- **Status:** Accepted
- **Date:** 2026-05-30
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

Secrets and keys across entelequia had grown across five mechanisms
(`pass`, Bitwarden, SOPS, raw GnuPG, ad-hoc SSH/deploy keys) with no
written statement of *which trust roots exist* and *what each one is
allowed to root*. The personal master GnuPG key had quietly become the
root of almost everything (git/mail signing, SSH auth, deploy subkeys,
`pass`, dotfiles SOPS), while a second, deliberately-separate identity
had emerged on the research side (`~/src/ivs-infrastructure`, see its
ADR-0003) — but the relationship between them was undocumented, and the
research identity's UID did not yet carry the institutional address.

Forces:

- **Blast radius.** One key rooting *everything* means one compromise
  loses everything. Separating identities by domain bounds the damage
  and keeps research-side recipients out of the personal key's history.
- **Identity bleed.** A personal `palomar.no` fingerprint appearing as
  a recipient on research infrastructure leaks personal identity into a
  work context (and vice versa).
- **Extensibility.** A future NTNU/OUS role key is foreseeable; the
  scheme must admit a fourth identity without rework.

## Decision

entelequia recognises **four classes of trust root**, enumerated in
`docs/source/keys-inventory.md` and `docs/source/secrets.rst`:

1. **Personal master** — `Rafael Palomar <rafael@palomar.no>`,
   `6513C7248D7BECE2EC1BD34B70350DAD507FA72F`. RSA-4096 `[C]`,
   air-gapped on the IronKey; `[S][E][SA]` daily subkeys. Roots
   git/mail signing, SSH auth, per-deploy SSH subkeys, `pass`, personal
   dotfiles SOPS (operator-only).
2. **IVS operator** — `8EADF28F4F8DC23942345E9A9440CF71CAEA2D0B`, a
   dedicated research-side key on curie. Roots `ivs-infrastructure`
   SOPS (host-side). Its agreed UID is **`Rafael Palomar
   <rafpal@ous-hf.no>`** — the institutional research address.
3. **Per-host SOPS keys** — one per fleet host, generated on the host,
   private half never leaves; roots that host's `/run/secrets`.
4. **Bitwarden master** — the human web/app vault, a separate root by
   construction (cloud, password-based).

Adding a future identity (e.g. NTNU) follows the same recipe: a
dedicated key with an institutional UID, an inventory row, and its own
SOPS recipient scope. Personal and research recipients **never** mix.

## Alternatives considered

### Alternative A — one key to root everything

Keep the personal master as the sole root, including research
infrastructure. Rejected: maximal blast radius, and it stamps the
personal `palomar.no` fingerprint onto research repos' `.sops.yaml` and
commit history — exactly the identity bleed we want to avoid.

### Alternative B — per-repository keys

A fresh key per project. Rejected: explodes the inventory, multiplies
the ceremony burden, and provides no benefit over per-*domain*
identities (personal / IVS / future-NTNU) at this scale.

## Consequences

**Easier:** the inventory has a small, closed set of human-meaningful
roots; "who can decrypt this?" is answerable per domain; a compromised
research key does not implicate personal signing.

**Harder / follow-up:**

- **Reconciliation done (2026-05-30):** the IVS operator key's primary
  UID is now `Rafael Palomar <rafpal@ous-hf.no>` (`gpg --quick-add-uid`
  + `--quick-set-primary-uid`); the old `ivs-sops@palomar.no` is kept as
  a secondary UID. The fingerprint is unchanged, so SOPS recipients are
  unaffected.
- **IVS-repo follow-up (open):** that repo's `keys-inventory.md` still
  shows the old UID, and hosts/keyservers hold the pre-reconciliation
  public key — distribute `/tmp/ivs-operator-pub.asc`. (`.sops.yaml`
  needs no change: it pins the fingerprint, not the UID.)

## Conformance

- The four roots are enumerated in `docs/source/keys-inventory.md`.
- `keys audit` (Phase 1) asserts every root in the inventory is present
  in (or correctly absent — `sec#` — from) the live keyring, and that
  no un-inventoried personal key roots a personal secret.

## References

- `docs/source/secrets.rst` — identity table + decision tree.
- `~/src/ivs-infrastructure/Docs/adr/0003`, `…/0006` — the research-side
  SOPS + inventory ADRs this mirrors.
- PKS: `~/pks/permanent/20260530T100020` (operator-only SOPS),
  `…T100021` (host-side SOPS).
