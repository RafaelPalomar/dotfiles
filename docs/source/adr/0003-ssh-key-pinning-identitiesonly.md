# 0003. SSH key pinning with `IdentitiesOnly`

- **Status:** Accepted
- **Date:** 2026-05-30
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

SSH authentication is served by the GnuPG agent (`enable-ssh-support`),
which presents **every** enabled `[A]` subkey to **every** server. The
personal master now has seven auth-capable subkeys (one primary `[SA]`
plus six per-deployment: monk, lovelace, edison, hopper, hamming,
baroja) and the count grows with the fleet.

OpenSSH servers cap authentication attempts at `MaxAuthTries` (default
6). The **hardened entelequia sshd sets it to 3.** When the agent offers
more keys than that cap before the correct one, the server aborts with
`Too many authentication failures` — a hard failure, not a fallback.
The current mitigation in `dotfiles/.gnupg/sshcontrol` is a fragile
hand-ordered comment ("most-frequently-used machines first") that does
not scale: with seven keys and a cap of three, four keys are
unreachable on a hardened host unless the ordering happens to be right.

## Decision

**Pin exactly one identity per host in the SSH client config.** For each
target, a `Host` block specifies the GnuPG agent socket, the single
public key to offer, and `IdentitiesOnly yes`:

```text
Host hamming
    IdentityAgent  /run/user/1000/gnupg/S.gpg-agent.ssh
    IdentityFile   ~/.keys/ssh/deploy/hamming.pub
    IdentitiesOnly yes
```

`IdentitiesOnly yes` + a pinned **public** key file (the private half
stays in the agent) makes the client offer only that key, so the server
sees exactly one attempt — never tripping `MaxAuthTries`, regardless of
how many keys the agent holds. Public deploy keys are emitted by
`keys deploy pubkey <name>` and stored under `~/.keys/ssh/deploy/`
(public → safe to version-control). The SSH config is rendered
declaratively via Guix home (Phase 2).

`sshcontrol` curation (enabling only needed keys) is retained as
defence-in-depth, but the hand-ordering hack is removed once pinning is
in place.

## Alternatives considered

### Alternative A — keep ordering `sshcontrol` by frequency

Order most-used keys first so they fall within `MaxAuthTries`. Rejected:
O(keys) fragility, breaks the moment a new key is added or usage shifts,
and *cannot* satisfy more than `MaxAuthTries` distinct hosts on a
hardened server.

### Alternative B — raise `MaxAuthTries` on the servers

Loosen the cap to fit the key count. Rejected: weakens every server's
brute-force posture to paper over a client-config problem, and still
fails as the key count climbs.

### Alternative C — one agent socket per identity

Run separate gpg-agents / sockets and select per host. Rejected:
heavyweight, fights `pam-gnupg`'s single-agent unlock model, for no gain
over per-host `IdentityFile` pinning.

## Consequences

**Easier:** the Nth deploy key costs one offer per host; adding a host
is "emit its `.pub`, add a `Host` block"; the `sshcontrol` ordering
comment and its maintenance burden disappear.

**Harder / follow-up:**

- Every deploy target needs a `Host` block — generated, not
  hand-written (Phase 2, Guix home).
- `~/.keys/ssh/deploy/*.pub` must stay in sync with the live deploy
  subkeys — `keys audit` cross-checks them against the inventory.

## Conformance

- **Asserts:** for every deployment in `keys deploy list`, a
  corresponding `~/.keys/ssh/deploy/<name>.pub` exists and the
  generated SSH config contains a `Host` block with `IdentitiesOnly
  yes` referencing it.
- A connection to a hardened host (`MaxAuthTries 3`) succeeds with N≥4
  deploy keys loaded in the agent.

## References

- `docs/source/secrets.rst` — "The SSH many-keys problem".
- `dotfiles/.gnupg/sshcontrol` — the in-tree scar comment that
  motivated this.
- `dotfiles/.local/bin/manage-deploy-keys.sh` — emits the public keys.
- `man ssh_config` — `IdentitiesOnly`, `IdentityFile`, `IdentityAgent`.
