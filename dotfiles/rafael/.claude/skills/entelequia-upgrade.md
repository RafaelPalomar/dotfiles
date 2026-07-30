---
name: entelequia-upgrade
description: Upgrade the entelequia Guix dotfiles fleet — move channel pins forward and roll them out. Use when the user says "upgrade entelequia", "bump the channels", "update the pins", "pull new guix", "reconfigure the fleet", or asks to update/deploy the dotfiles system. Encodes the channels.scm (intent) → channels-lock.scm (pins) regen, the validation gate, per-machine dry-run/build, and the ordered fleet rollout (curie first, then the rest) with the known deploy gotchas. Never edits channels-lock.scm by hand; never combines deploy + reboot.
---

# entelequia-upgrade

Move the entelequia fleet forward. An "upgrade" is: bump the channel
pins, prove the configs still evaluate/build, then roll the new pins
out machine by machine. All from `~/.dotfiles`.

## Mental model

- **`channels.scm`** = intent — channel names, URLs, branches,
  introductions. Hand-edited only to add/remove a channel or change a
  branch/URL.
- **`channels-lock.scm`** = the lock — exact commit per channel. This
  is what **every** deploy / reconfigure reads via `guix time-machine
  -C channels-lock.scm`. **Never hand-edit it** — it is machine-generated.
- Upgrading = regenerating the lock from intent (latest commit of each
  branch), then reconfiguring each machine against the new lock.
- System and home are **decoupled**. A system reconfigure/deploy touches
  kernel/services/drivers only; the per-`(machine,user)` Guix Home is a
  separate `guix home reconfigure`. Bump both when pins move.

## Procedure

Always run steps 1–3 first — they are non-destructive. Confirm scope
with the user before step 4 (deploys change live machines).

### 1. Bump the pins (regenerate the lock)

```bash
cd ~/.dotfiles
./scripts/update-lock.sh
```

This runs `guix time-machine -C channels.scm -- describe -f channels`
to resolve every branch to its newest commit, sanity-checks that the
new lock covers exactly the channels declared in `channels.scm` (no
phantom/missing channels), and rewrites `channels-lock.scm`. It prints
the pin movement. Building the fresh guix here can take 10–25 min on a
cold cache (longer on the X220 / baroja).

Review the diff — `git diff channels-lock.scm`. Every changed
`(commit ...)` is a channel that moved. If a channel you did not expect
moved, or an agent/private channel jumped a lot, mention it before
proceeding.

To pin a **single** channel (e.g. only guix) rather than everything,
still regenerate via the script, then `git checkout channels-lock.scm`
the lines you did not intend to move — but the default is bump-all.

### 2. Validate (fast, no builds)

```bash
./scripts/validate-refactor.sh
```

Tier-1 gate (~1 min under the new lock): checks intent↔lock drift, loads
the core lib/suite modules, and evaluates every system machine, home
machine, and VM config to the right record type. Must be green before
any build. On failure it prints the first real error line per file.

### 3. Dry-run build per machine (no sudo, no apply)

Prove the new pins actually build before touching hardware. At minimum
build the machine you're about to deploy:

```bash
guix time-machine -C channels-lock.scm -- system build -L . \
  entelequia/system/machines/<machine>.scm --dry-run
```

For a real closure check drop `--dry-run` (builds/downloads everything).
Optionally build the home too:

```bash
guix time-machine -C channels-lock.scm -- home build -L . \
  entelequia/home/machines/<host>-<user>.scm
```

### 4. Roll out the fleet (ordered, one machine at a time)

**Deploy laptop first, soak, then the rest.** Preferred order:

1. **curie** (this laptop) — easiest to roll back, catches regressions early.
2. **einstein** (desktop) — after curie proves stable.
3. Remote boxes: **hopper**, **baroja**, **alucard**, **lovelace**, **edison**.

Let curie run 24–48h before pushing pins to the desktop/servers unless
the user wants everything now.

**Local machines (curie / einstein)** — reconfigure system, then home:

```bash
# from a shell on the machine itself
sys-reconfigure     # sudo guix time-machine -C lock -- system reconfigure ...$(hostname).scm
home-reconfigure    # guix time-machine -C lock -- home reconfigure ...$(hostname)-$(whoami).scm
```

(Or `./scripts/deploy.sh <machine>` for the system half; it prompts
before applying and accepts `--dry-run`.) `sys-update` / `home-update`
do a `git pull` first. Roll back with `sudo guix system roll-back` /
`guix home roll-back`.

**Remote machines (guix deploy)** — system only, from curie:

```bash
./scripts/deploy.sh <edison|hopper|baroja|alucard|lovelace> --dry-run
./scripts/deploy.sh <edison|hopper|baroja|alucard|lovelace>
```

which wraps `guix time-machine -C channels-lock.scm -- deploy -L . \
entelequia/deploy/<target>.scm`. Their home envs reconfigure separately
(over SSH on the box, or via that host's `home-reconfigure`).

### 5. Commit

Commit `channels.scm` + `channels-lock.scm` **together**, one commit,
summarizing the notable pin moves (e.g. `channels-lock: bump guix,
nonguix; xlibre-server X`). Prefer rebase/linear history.

## Deploy gotchas (fleet-specific, from hard experience)

- **Always deploy, then reboot separately.** `guix deploy -x -- reboot`
  or combining the two can drop SSH mid-deploy. Reboot as its own step.
- **guix deploy auth**: guile-ssh tries gpg-agent keys before the
  configured `(identity ...)`. For on-disk-key targets like **alucard**,
  prefix the command with `SSH_AUTH_SOCK=`. Never pass `-i ~/.ssh/...`
  — the SSH agent selects the key. If a host bans you mid-deploy:
  `fail2ban-client unban <ip>` on that host.
- **ClientAlive timeout**: a deploy can drop at "send-files / Socket
  error: disconnected" when a long local build outlasts sshd's
  ClientAliveInterval. Not a ban — just re-run; the warm cache finishes it.
- **First deploy of a freshly-installed (systole) box** moves sshd
  22→2222 and wedges the live session — a reboot is mandatory to land
  it, then reconnect on **2222** with the pinned key. `MaxAuthTries` is
  20 fleet-wide, so sshcontrol key order no longer gates deploys.
- **Never `--no-verify`, never force-push, never skip hooks** without
  an explicit ask.
- **edison / lovelace containers are rootless under rafael.** After a
  reboot, `herd start` can hit "name in use"; the fix is per-pair
  `stop + rm + start`, not another reboot. `podman pull` must run as
  rafael (`sudo -u rafael -i podman pull` or `herd action <svc> pull`).
- **NVIDIA hosts (einstein, alucard)** need the three-layer GL fix
  (xlibre glx disable + libglvnd in home + LD_LIBRARY_PATH) — a pin
  bump that changes mesa/xlibre can re-expose it; watch GL apps.

## Notes

- entelequia is registered in PKS (`~/pks/projects/20260421T194720`).
  A channel bump is routine — not log-worthy. A pin bump that forces an
  architectural change, or a channel add/remove, is worth a
  `pks-project-log` entry.
- If `update-lock.sh` reports channel-set drift, the fix is to reconcile
  `channels.scm` — do **not** hand-patch the lock to match.
- Stale/force-pushed upstream pins: see
  `reference_channels_lock_pin_diagnosis` memory + PKS permanent
  `20260426T203605` for recovery.
