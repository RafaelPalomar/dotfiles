# 0005. Separate home from work by per-machine role, structurally

- **Status:** Accepted
- **Date:** 2026-06-02
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

The fleet shares one `~/.dotfiles` (entelequia) repo across home and
work-use personal machines. **baroja** (ThinkPad X220, `192.168.88.117`)
is designated the new home daily-driver, but it was added as a hopper
clone (commit `57f7da0`) with no role marker: it imports
`(entelequia home profiles email)`, ships `email-home-packages`, and —
the real leak — receives the store-deployed shared `dotfiles/.mbsyncrc`
(both work O365 OAuth pass-refs `email/ntnu.no` + `email/ous-research.no`)
via `home-dotfiles-service-type`, which is pushed to *every* machine.
Dropping the email *profile* alone does **not** remove that shared
dotfile. ADR-0001 mandates identity non-mixing; the two-PKS-roots design
(PKS `20260507T100903`) mandates **physical** separation — personal data
must not reside on a work disk at all. Personal mail is
`rafael@palomar.no` on **Tuta**, which has no IMAP/SMTP/CalDAV/bridge
(verified 2026), so it is human-only and needs no local mail stack.

## Decision

Encode domain as a declarative **per-machine role**. Add
`entelequia/home/profiles/role.scm` exporting `(home-role 'home | 'work)`
which gates (a) the email profile, (b) the `tailscale-work` import, and
(c) a `~/.config/entelequia/role` marker for userspace tooling. baroja =
`(home-role 'home)`; curie + einstein = `(home-role 'work)`. Split
`dotfiles/.mbsyncrc` → `.mbsyncrc.work` (the two O365 accounts) +
`.mbsyncrc.personal` (**empty** — Tuta is human-only), installed by role;
same for `.msmtprc` / `.notmuch-config(-agent)`. Make the cut structural
at the secret layer: baroja gets a **separate pass store / GPG identity**
(or the work-email subtree is pruned and the work GPG private key proven
absent). baroja gets its **own personal NextCloud account** as an
independent PKS sync surface, distinct from curie's.

## Alternatives considered

- **Drop the email profile only.** Rejected: the shared store-deployed
  `.mbsyncrc` (with both OAuth pass-refs) still lands on baroja — the
  leak is the dotfile, not the profile.
- **Split `~/.dotfiles` into home/work repos.** Rejected by the user: one
  shared repo is kept; separation is per-machine
  config/data/identity/tailnet/pass-store, not a repo boundary. Work
  *infrastructure* (hamming/monk) already lives in the separate
  `ivs-infrastructure` repo/root.
- **Logical separation (disk encryption, same account).** Rejected per
  PKS `20260507T100903`: encryption does not remove the data; backups,
  compromise, agent reads, and screen-sharing remain leak surfaces.

## Consequences

baroja physically cannot attempt work-mail auth (no referenced config, no
decryptable secret). curie's cleanup of `{fleeting,archive}/personal` is
safe because baroja syncs a *different* NextCloud account (no shared
tree) — but the cleanup must follow the migration ordering (out-of-band
backup → sha256 denote-ID manifest match → delete only after baroja is
verified holding the data). Adds `role.scm` + per-role dotfile variants
to maintain. Follow-up: add the SSH `Host` block for baroja
(`192.168.88.117:2222`, `IdentitiesOnly yes`, pinned `IdentityFile`),
resolving the `config.d/README.md:36` TODO.

## Conformance

`(home-role 'home)` machines must not install `email-home-packages`, must
not import `tailscale-work`, and must have **zero** work OAuth entries in
their pass store — a verification step asserts `pass ls` shows no
`email/ntnu.no` / `email/ous-research.no` and that `mutt_oauth2.py` /
`msmtp` are absent. Denote IDs are preserved byte-for-byte across the PKS
migration.

## References

ADR-0001 (trust roots & identities); PKS `20260507T100903` (two-PKS-roots
isolation); PKS `20260421T194720` (entelequia project log);
`~/.claude/plans/family-agentic-architecture.md` §2.1 + §6-B.
