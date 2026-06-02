# 0007. Ops-deploy approval gate — a scoped deviation from the human-keystroke invariant

- **Status:** Accepted
- **Date:** 2026-06-02
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

The hard invariant (PKS `20260422T145624`, encoded in `CLAUDE.md`) puts
send / activation authority on a **human keystroke the gateway cannot
forge** — mail `C-c C-c`, `ntnu-vpn-up` 2FA, GOG login. **Mr. Robot**
(ops) needs to drive `guix deploy` across the home fleet. The user
explicitly chose **"approval-gated in-channel"** — a parents-only approve
button in `#ops` that *is* the activation gate (Mr. Robot executes the
deploy on approval) — **over** the reviewer's recommendation to require an
out-of-band keystroke for any non-edison activation. This ADR records
that as a conscious, bounded exception rather than an accident.

## Decision

Accept the in-channel approve button as the activation gate for
**guix-deploy of the home fleet only**, as a conscious, scoped deviation
from the human-keystroke invariant. Mitigations — **all mandatory and
enforced before the button acts**:

1. **Parents-only approval** — only **Maria's** and **Rafael's**
   Mattermost IDs may approve; any other approve is rejected + logged.
2. **Mandatory dry-run + diff first** — `guix deploy --dry-run` output +
   generation diff posted to `#ops`; the approve button is not rendered
   until the plan is in-channel.
3. **Every activation logged** append-only — approver, triggering message
   id, `(host,config)` pin, resulting generation.
4. **`channels-lock` pinned** — activation bound to a pinned
   channels-lock; no floating channels.
5. **Automatic rollback / generation-pin on activation failure** — a
   failed activation auto-rolls-back to the prior pinned generation
   (fails closed).
6. **Scope = guix-deploy home fleet only** — never routers (read-only
   diagnostics only, ADR-0006), never work infra (separate ivs root).

## Alternatives considered

- **In-channel prepares, human keystroke activates (reviewer's rec).**
  The bot stages + shows the diff but the actual activation runs from an
  out-of-band keystroke. Strictly honors the invariant; rejected by the
  user as too much friction for routine home-fleet deploys.
- **Edison-self autonomous, rest keystroke.** Rejected: the user wanted
  the convenience across the fleet, gated by approval.
- **Fully autonomous scoped deploy.** Rejected: no human in the loop is
  an unacceptable blast radius.

## Consequences

A compromised gateway or prompt-injected ops agent could in principle
forge an approve message; the parents-only check, mandatory dry-run/diff,
append-only logging, channels-lock pin, and auto-rollback bound the blast
radius. The human-keystroke invariant remains the **default everywhere
else** (mail send, VPN, GOG). The router and work-infra exclusions are
load-bearing — this gate must never widen to them without a new ADR.
Revisit if a viable gateway-compromise path emerges.

## Conformance

The approve handler rejects non-parent approver IDs and logs the
rejection; the approve button does not render until the dry-run+diff is
in-channel; a failed activation auto-rolls-back to the prior pinned
generation. The scope allow-list contains only guix-deploy home hosts —
a test asserts routers and ivs hosts are absent.

## References

PKS `20260422T145624` (agent send authority belongs on a human
keystroke); ADR-0006 (the deploy identity this gate drives); `CLAUDE.md`
hard rules; `~/.claude/plans/family-agentic-architecture.md` §2.2 + §6-A.
