# 0006. Family-assistant agent identities — least-privilege, separately revocable

- **Status:** Accepted (`keys agent` tooling is a Phase-5 deliverable)
- **Date:** 2026-06-02
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

The Hermes family assistant runs three personas as distinct
capability envelopes: **Mary Poppins** (household), **Arquimedes**
(tutor, kid-safe), **Mr. Robot** (ops). Each needs an attributable,
least-privilege identity. Mary Poppins needs read/write on the family's
NextCloud data; Arquimedes needs scoped access to per-kid school folders
*only*; Mr. Robot needs to drive `guix deploy` but must never hold the
personal-master key (air-gapped per ADR-0004) and must be revocable
without an IronKey ceremony. NextCloud runs on **lovelace**; the agents
on edison are clients over the drake-karat tailnet. Routers are a
catastrophic write-blast-radius.

## Decision

- **Per-agent NextCloud users with SOPS app-passwords.**
  `mary-poppins` — CalDAV + Deck + WebDAV `rw` on `/Family`, `r` on
  `/Kids`, per-member allow-list; **never** rafael's PKS tree.
  `arquimedes` — WebDAV `rw` on `/Kids/Leandro/schoolwork` +
  `/Kids/Adrian/schoolwork` **only** (no calendar/Deck/Family reach).
  **Mr. Robot has no NextCloud user** (infra-only).
- **Mr. Robot's deploy identity** is a born-online plain **ed25519**
  keypair with role UID `hermes-ops-deploy` — **not** a GPG `[A]` subkey
  of the personal master. Private half: SOPS → tmpfs
  `/run/secrets/hermes-ops/deploy_ed25519` (`#o400`, never `/gnu/store`,
  never the writable `HERMES_HOME`), `ssh-add`ed into a container-scoped
  agent. Host-side `authorized_keys` is **force-commanded**, `from=`-pinned,
  **no-pty**, bound to a pinned-`channels-lock` `guix activate` of a
  pre-agreed `(host,config)` — never an interactive root shell. Issued /
  rotated / revoked only via a **net-new `keys agent` subcommand**
  (Phase 5) that writes the inventory row + `.sops.yaml` recipient +
  `authorized_keys` stanza + per-host pin in one commit, with an
  `issue→revoke` acceptance test.
- **Routers: read-only diagnostics only.** A dedicated,
  separately-revocable **READ-ONLY** token (status / logs / DHCP leases)
  — never write; a different credential and code-path from the deploy
  key. Router config stays fully human.

## Alternatives considered

- **Reuse the personal-master `[A]` deploy subkeys for Mr. Robot.**
  Rejected: violates ADR-0004 (master air-gapped) and ADR-0001 (identity
  non-mixing); a compromise would touch the master and need an IronKey
  ceremony to rotate.
- **One shared family-bot NextCloud account.** Rejected: no per-agent
  isolation or attribution; a tutor compromise would reach Family data.
- **Agents impersonate rafael.** Rejected: full access, no isolation, no
  attribution.
- **Let Mr. Robot manage router config.** Rejected: routers aren't
  guix-deploy targets and are the largest write-blast-radius; downgraded
  to read-only diagnostics; any change is its own ADR + sign-off.

## Consequences

Compromise of any one agent identity is bounded and independently
revocable; the personal-master key is never on the network; router
*mutation* authority simply does not exist for any agent. `keys agent`
is net-new and on the critical path for the deploy identity (Phase 5) —
no deploy identity is issued by hand before it exists. NextCloud
app-password determinism plus a manual DB-role step may force a one-time
human capture (runbook-documented).

## Conformance

`keys audit` must recognise agent identities and pass an `issue→revoke`
round-trip (revoke removes the `authorized_keys` force-command line).
Every agent NextCloud user appears in the keys inventory with its scope.
No agent identity is a copy/relocation of the personal master (ADR-0004).
A grep of the router credential shows read-only / diagnostics only —
never a write path.

## References

ADR-0001 (trust roots & identities), ADR-0003 (SSH pinning), ADR-0004
(master air-gap); ADR-0007 (the deploy approval gate that drives this
identity); PKS `20260421T194720`;
`~/.claude/plans/family-agentic-architecture.md` §2.2 + §2.3 + §6-C.
