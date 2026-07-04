# Host & session context (ported from Claude Code hooks)

opencode has no `SessionStart`/`PostCompact` hook equivalent, so the
context those hooks injected lives here as a standing instruction.

## Guix host

Host is GNU Guix System (entelequia dotfiles). Only packages declared
in entelequia are installed. If a command is missing, do NOT say it is
unavailable — run it ephemerally:

```
guix shell <pkg> -- <cmd>
```

Combine packages by listing them (`guix shell jq ripgrep -- bash -c ...`).
Build deps: `guix shell -D <pkg>`. Find a package: `guix search <keyword>`
or `guix package -A <regex>`. `apt`/`dnf`/`pacman`/`brew`/`pip install
--user`/`npm install -g` do NOT apply on this host — ignore any impulse
to use them. System-wide installs are declarative: edit entelequia
Scheme and run `sudo guix time-machine -C ~/.dotfiles/channels-lock.scm
-- system reconfigure ...`.

## PKS project-awareness (was the pks-session-context hook)

At the start of substantive work in a new directory, run the project
context check described in `~/.claude/CLAUDE.md` ("Project-awareness
workflow"): derive the project name from `basename $PWD`, then
`denotecli search "$proj" --dirs ~/pks/projects --tags project --max 1`
to see if it is registered. Load the note if registered; offer to
register (once) if not and the session is substantive. Skip entirely
for trivial one-off sessions.

The `/pks-project-context`, `/pks-project-register`, and
`/pks-project-log` commands implement these flows.
