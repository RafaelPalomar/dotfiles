# 0002. The pass-vs-SOPS boundary

- **Status:** Accepted
- **Date:** 2026-05-30
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

Three secret stores were in use with overlapping, undocumented scope:
Bitwarden, `pass`, and SOPS. The Bitwarden boundary was already clear in
practice (human, cross-device, mobile/browser autofill). The genuine
ambiguity was **inside the programmatic lane**: `pass` and SOPS are
*both* GnuPG-rooted machine-secret stores, and "does this new secret go
in `pass` or SOPS?" had no rule. The cost was cognitive — every new
script's secret was a fresh decision.

The de-facto practice, surfaced in conversation: `pass` for systems
*used/operated directly* (ad-hoc, imperative — `pass insert`, then a
sync script materialises an envfile); SOPS for systems that are
`guix deploy`'d (structured, planned, committed to git, possibly
multi-host).

## Decision

Place every secret with **two questions, asked in order** (rendered as
the decision tree in `docs/source/secrets.rst`):

1. **Is it for a human to use across devices?** → **Bitwarden** (cloud).
2. Otherwise it is for a machine/script. **Will the consuming system be
   `guix deploy`'d?**
   - **No** (used/operated directly, ad-hoc) → **`pass`**, materialised
     to an envfile by `keys sync`.
   - **Yes** (structured, planned, in git) → **SOPS**, in the
     operator-only pattern for personal dotfiles or the host-side
     pattern for a fleet host.

**Grey-zone rule:** when a directly-used box graduates to
`guix deploy`, its secret graduates `pass` → SOPS. This is a documented,
mechanical move, not a re-deliberation.

## Alternatives considered

### Alternative A — collapse to one programmatic store

Drop either `pass` or SOPS. Rejected: they answer different questions.
`pass` is an imperative per-operator workstation tool (perfect for
"paste an API key once, have a script read it"); SOPS is a build-system
primitive that puts *encrypted* secrets in git and decrypts at
edit/activation time (perfect for reproducible `guix deploy`). Forcing
one to do the other's job either puts plaintext in `/gnu/store` (SOPS's
job done by `pass`) or burdens every ad-hoc secret with a `.sops.yaml`
recipient edit + commit (`pass`'s job done by SOPS). The IVS ADR-0003
rejected `pass` for deploy secrets for exactly this reason.

### Alternative B — leave the boundary tribal

Keep doing it by feel. Rejected: that *is* the fragmentation complaint.
The tools were fine; the absence of a written rule was the defect.

## Consequences

**Easier:** a one-line answer for where any secret goes; the grey zone
has an explicit migration rather than a judgement call.

**Harder / follow-up:**

- Existing secrets should be spot-checked against the rule; any
  mis-filed secret migrates. (No known violations today; the wiring of
  email OAuth — refresh tokens in `pass`, client id/secret in dotfiles
  SOPS — is consistent with the rule.)
- `keys` should grow a `pass`→SOPS graduation helper (deferred).

## Conformance

- The decision tree is published in `docs/source/secrets.rst`.
- The "cases covered" table there assigns each of the 14 known cases to
  exactly one lane — a checklist a reviewer can apply to a new secret.

## References

- `docs/source/secrets.rst` — decision tree + cases-covered table.
- `~/src/ivs-infrastructure/Docs/adr/0003` (Alternative B there rejects
  `pass` for deploy secrets).
- PKS: `~/pks/permanent/20260530T100020` / `…T100021` (SOPS patterns).
