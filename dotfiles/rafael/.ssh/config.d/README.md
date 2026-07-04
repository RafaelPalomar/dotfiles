# ~/.ssh/config.d/ — domain-specific pinned hosts

`~/.ssh/config` ends its global section with `Include ~/.ssh/config.d/*.conf`.
Drop a `*.conf` here to pin hosts whose connection details should **not**
live in the public `RafaelPalomar/dotfiles` repo — notably the IVS / OUS
hosts on internal `10.x` addresses.

Files matching `*.conf` are included; this `README.md` is not.

The **public deploy keys** for these hosts are still operator-owned and
live in `~/.keys/ssh/deploy/` (committed to dotfiles); only the
*connection params* (internal IP, port) are kept out of the repo.

## Pinning template (ADR-0003)

Create e.g. `~/.ssh/config.d/ivs.conf` (managed by the IVS repo or kept
local-only) — params mirror `~/src/ivs-infrastructure/deploy/<host>.scm`:

```
Host hamming
    HostName <hamming internal IP>
    User root
    Port 2222
    IdentityAgent SSH_AUTH_SOCK
    IdentityFile ~/.keys/ssh/deploy/hamming.pub
    IdentitiesOnly yes

Host monk
    HostName <monk internal IP>
    User root
    Port 22
    IdentityAgent SSH_AUTH_SOCK
    IdentityFile ~/.keys/ssh/deploy/monk.pub
    IdentitiesOnly yes

# baroja: deploy key exists; add a block once its host is defined.
```

`IdentitiesOnly yes` + the single pinned `IdentityFile` is what keeps the
agent from offering all its keys and tripping `MaxAuthTries`.
