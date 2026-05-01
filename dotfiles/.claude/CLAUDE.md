# User-level Claude Code instructions

Global instructions that apply in every project directory.  A
project-specific `CLAUDE.md` at the repo root overrides these.  Per-silo
`CLAUDE.md` files inside `~/pks/` layer on when you are working there.

## Personal Knowledge System (PKS)

The user maintains a function-based Zettelkasten at `~/pks/` (symlinked
into `~/Nextcloud/PKS/`) with silos: `fleeting/`, `permanent/`,
`literature/`, `projects/`, `reference/`, `review-queue/`, and
`library/` for raw material (PDFs, scans, `references.bib`).

Notes use the Denote filename convention:

```
YYYYMMDDTHHMMSS--slug__kw1_kw2.org
```

Closed keyword vocabulary (warn before extending):
`_research _code _learn _project _lit _perm _fleeting _ntnu _ous
_agenda _moc _meeting _hub _idea _review`.

### Tools available

- `denotecli` — structural queries (search / read / graph / tags /
  timeline / create / rename). Prefer this over `rg` on filenames.
  Use `--dirs ~/pks/<silo>` to scope searches.
- PKS Claude Code skills (`pks-*.md` under `~/.claude/skills/`) — use
  these as the primary entry points; they encode safety rules.

### Safety rules (always)

1. **Never regenerate denote IDs.** They are load-bearing for
   backlinks. Use `--keep-id` on any rename.
2. **Never bulk-delete or bulk-rename.** Single-note operations only.
3. **Always confirm** before creating, renaming, or moving notes.
4. **Default silo is `fleeting/`** — new notes land there unless the
   user specifies otherwise.
5. **Memory vs PKS**: ephemeral session context → auto-memory; durable
   knowledge → PKS project/permanent notes. Never duplicate full
   content in both — memory holds a short pointer to the PKS note ID.

## Project-awareness workflow

At session start in a new working directory, run `pks-project-context`
(the skill handles detection + routing):

1. Derive project name from `basename $PWD`.
2. `denotecli search "$proj" --dirs ~/pks/projects --tags project --max 1`
   to check if the project has a registered denote note.
3. **Registered** — load the note (`denotecli read <ID> --dirs
   ~/pks/projects --outline`) and carry Status, Log, Architecture,
   References as implicit session context. Do not re-announce on every
   turn.
4. **Unregistered** — **ask once per session**: "This project
   (`$proj`) isn't in your PKS. Want me to register it? (Worth it for
   sustained projects; skip for one-offs.)"
   - On **yes**: invoke `pks-project-register` which creates
     `~/pks/projects/<ID>--<slug>__project_agenda_<domain>.org` from
     the denote project template.
   - On **no**: remember the decline for this session only. Do not
     nag. Do not persist a "never register" marker.
5. **Trivial sessions** (user asked for `git log`, `ls`, a single
   command, a one-shot question) — skip the registration prompt
   entirely.  Only prompt when about to make substantive changes.

## Log-worthy events

During substantive work in a registered project, offer
`pks-project-log --id <ID> --summary "…"` when one of these occurs:

- Architectural decision with explicit justification.
- Rejected approach — document why, saves future re-deliberation.
- Pattern crystallising across multiple touchpoints.
- Discovery that changes how the project is understood.
- Non-obvious constraint or invariant revealed.

**Not log-worthy**: routine task completions, commit messages, linter
fixes, trivial refactors. Git history carries those.

Each log entry: one line + 1-3 sentences of context + optional
`[[denote:ID]]` to a permanent/literature note. User confirms before
append.

## Memory coordination

When logging to the PKS, consider whether a companion auto-memory
entry is warranted:

- **Feedback**: user correction or confirmation of approach → memory
  (`feedback_*.md`), plus PKS Log if it's a durable architectural
  decision.
- **Project state**: current blockers, in-flight decisions → memory
  (`project_*.md`) with a **pointer** to the PKS note, not a
  duplicate.
- **Reference**: pointers to external systems (Linear, Grafana) →
  memory (`reference_*.md`).
- **User profile**: role, preferences → memory (`user_*.md`).

PKS holds full context + reasoning. Memory holds short, quickly-indexed
pointers.

## Mail capability (available in every project)

The user has a local-first mail stack: **mbsync** (human-only) syncs
OAuth2-enabled Office 365 mailboxes to `~/.local/share/mail/`; **mu4e**
is the human UI in Emacs; **notmuch** is the full human-side index.

The agent does **not** read `~/.local/share/mail/` or run `notmuch`.
It queries a separate, filtered index via **`notmuch-agent`**, whose
database at `~/.local/share/mail-agent/` is a symlink tree containing
only allowlisted folders (INBOX, Archive, Drafts, Sent, Sent Items,
Kladd).  The **Sensitive** folder — and every other folder not on the
allowlist — is **physically absent** from the agent's DB.  No query
flag (including `--exclude=false`) can surface it.

The agent has **read (via `notmuch-agent`) + draft (via `mail-draft`)**
authority only — never send, never sync, never touch OAuth.  Send
always stays on a human `C-c C-c` in mu4e.

### When the user asks for mail work

Trigger phrases: *"draft an email to …"*, *"reply to the thread about
…"*, *"what's in my inbox"*, *"find the message from … about …"*,
*"summarize that thread"*, *"prepare a response …"*.

Do NOT treat such requests as "write me a markdown file".  The two
entry-point skills are documents under `~/.claude/skills/`:

- **`~/.claude/skills/mail-triage.md`** — read / search / summarize /
  tag via `notmuch-agent --format=json`.  Read this file when the user
  asks about the content of their mail.
- **`~/.claude/skills/mail-draft.md`** — compose a draft and write it
  as a Maildir file.  Read this file when the user asks you to draft.

Both skills wrap a single helper script:
`~/.local/bin/mail-draft` (Python) — produces an RFC-5322 file at
`~/.local/share/mail/<account>/Drafts/cur/<uniq>:2,DS`.  On the user's
next `sync-mail`, `mbsync` pushes it to IMAP and the user opens it in
Outlook / mu4e / phone to review and send.

Quick invocation example (reply):

```bash
echo "$body" | mail-draft \
  --account ous \
  --to "person@example.com" \
  --subject "Re: …" \
  --in-reply-to '<msg-id>' \
  --references '<prev> <msg-id>'
```

Accounts are `ous` (rafael.palomar@ous-research.no) or `ntnu`
(rafael.palomar@ntnu.no) — chosen by context of the thread or by the
user explicitly.

### Routine workflow

**Syncing is a human step.** `sync-mail`, `mbsync`, and
`sync-mail-agent-index` are all denylisted for the agent.  If the user
asks for fresh data, ask them to run `sync-mail` in their shell; do
not attempt to work around the deny.

**After writing a draft**: tell the user the draft path and remind
them to run `sync-mail` when convenient — that will upload the draft
to IMAP Drafts and their phone/Outlook will see it.  Until they run
it, the draft lives only locally.

**Before fresh mail triage**: if the user's question implies recency
("what's in my inbox *now*", "any new mail from X?"), ask them to
run `sync-mail`.  For retrospective queries on indexed history, the
agent's current index is fine.

### Hard rules (enforced by `~/.claude/settings.json` deny-list)

1. **Never call** `msmtp`, `sendmail`, `mutt_oauth2.py`, or any
   `auth-email-*` alias.  Denylisted.  **Send authority belongs on a
   human `C-c C-c` in mu4e or the Send button in Outlook** — never
   the agent.
2. **Never call** raw `notmuch`, `mbsync`, `sync-mail`, or
   `sync-mail-agent-index`.  Denylisted.  The only permitted notmuch
   entry point is `notmuch-agent` (read/search/tag only).  The agent
   does not refresh its own index; the user's `sync-mail` does that
   atomically.
3. **Never read** `~/.local/share/mail/**`, `~/.local/share/mail-agent/**`,
   `~/.notmuch-config`, or `~/.notmuch-config-agent` with the `Read`
   tool.  All mail data flows through `notmuch-agent`'s stdout.
4. **Never read** `~/.password-store/**` or `~/.dotfiles/sops/**`.
   Both are denylisted.
5. **Never write to** `~/.local/share/mail/**` or
   `~/.local/share/mail-agent/**` directly.  Use `mail-draft`, which
   is the one permitted writer.
6. **The Sensitive folder is invisible by design**, not by policy.
   It is not indexed in the agent's DB.  If the user asks about
   content that would only live there, tell them you cannot see it
   and suggest they read it in mu4e themselves.
7. If the user wants to *send* after review: remind them to open the
   draft in mu4e and press `C-c C-c`, or use Outlook's Send button.
   Do not offer to send it yourself.

### See also

`~/pks/permanent/20260422T145624` — *Agent send authority belongs on a
human keystroke* (the rationale behind these rules).
`~/pks/permanent/20260422T145645` — *JSON machine surface alongside
the human UI* (why notmuch + mu4e both exist).

## Research capability (available in every project)

The user has local-first CLI wrappers for academic literature search
and retrieval:

- **`paper-search`** — queries Scopus (needs NTNU VPN + `pass show
  apis/scopus`) *and* OpenAlex (always available, no auth); emits
  JSON lines by default or human-readable output with `--pretty`.
  Dedupes by DOI.  Falls back to OpenAlex alone when the VPN is
  down or no Scopus key is configured.
- **`paper-fetch`** — resolves a DOI, tries Unpaywall for a legal
  open-access PDF, then falls back to the publisher via `doi.org`
  through the NTNU VPN.  PDFs land in `~/pks/library/<slug>.pdf`
  (the user's Zettelkasten library silo).
- **`ntnu-vpn-status` / `ntnu-vpn-up` / `ntnu-vpn-down`** — VPN
  management.  The `-up` step always needs a human because Feide 2FA
  OTP cannot be automated.  Never run `ntnu-vpn-up` from the agent;
  if the VPN is down and paywalled material is needed, ask the user
  to bring it up.

### When to use

Trigger phrases: *"find papers on …"*, *"what's been written about
…"*, *"papers by <author>"*, *"pull / download / get paper <DOI>"*,
*"add this paper to my library"*, *"is DOI X in my library?"*.

Do NOT treat such requests as general `WebSearch` work — the
entry-point skill is `~/.claude/skills/research-papers.md`, which
encodes the Scopus quota / ToS rules, the VPN preconditions, and
the PDF landing convention.  Read that file when the user asks for
anything matching these triggers.

### Hard rules

1. The Scopus API key at `pass show apis/scopus` is read by
   `paper-search` itself — do not echo, paste, or otherwise
   exfiltrate it.
2. Scopus ToS forbids bulk caching or redistribution.  Interactive
   single-query use is fine; scraping (*"fetch everything citing X
   since 2020"*) requires a warning + cap.
3. Never auto-run `ntnu-vpn-up` — the 2FA OTP step belongs on a
   human keystroke, same category as mail `C-c C-c`.
4. Downloaded PDFs go to `~/pks/library/` only.  Do not modify
   `references.bib` unless the user explicitly asks — they curate it
   manually.

## GOG library capability (available in every project)

The user has **`lgogdownloader`** (nonguix channel, installed via the
gaming home profile) for driving their GOG.com game library from the
CLI.  The agent's authority is read + download; auth is human-only.

- **`lgogdownloader --check-login-status`** — check session state.
- **`lgogdownloader --list games`** / **`--list json`** — list owned
  titles.
- **`lgogdownloader --download --game "^<slug>$" --platform 4
  --directory ~/Games/gog-installers`** — pull a Linux installer.
- After download, hand off to **`gog-install <installer.sh>`** and
  follow the `games.scm` + `gaming-home-packages` packaging flow.

### When to use

Trigger phrases: *"what's in my GOG library?"*, *"install / download
/ get <game> from GOG"*, *"do I own <game> on GOG?"*, *"update my GOG
installers"*.

Entry-point skill: **`~/.claude/skills/gog-library.md`** — encodes the
auth precondition, download layout, packaging handoff, and bulk
caveats.  Read that when the user matches these triggers.

### Hard rules (enforced by `~/.claude/settings.json` deny-list)

1. **Never run `lgogdownloader --login`, `--gui-login`, or
   `--browser-login`.**  The OAuth paste-back is a human keystroke —
   same category as mail `C-c C-c` or `ntnu-vpn-up`.  Denylisted.
2. **Never read or write `~/.config/lgogdownloader/**`.**  The token
   file lives there.  Denylisted.
3. Downloads go to `~/Games/gog-installers/` only.  Do not invent
   other paths.
4. Do not bulk-download the full library without explicit user ask;
   GOG's API is unofficial and can throttle.  Cap at 3-5 per session
   when in doubt.
5. Strip account/email/identity fields from `lgogdownloader` output
   before pasting into chat.  Library titles fine, account identity
   not.

## Interaction style

- Terse by default — the user reads diffs, no trailing summaries of
  routine changes.
- Present exploratory questions as 2-3 sentence recommendations, not
  decided plans.
- Prefer rebase over merge (linear history preference).
- Never skip hooks (--no-verify) or force-push without explicit ask.
