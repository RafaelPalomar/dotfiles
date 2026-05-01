---
name: gog-library
description: Drive the user's GOG.com library via `lgogdownloader` — list owned games, download installers, and hand off to `gog-install` + `games.scm` for declarative packaging. Use when the user asks what's in their GOG collection, asks you to install / download / get a specific game from GOG, or wants to update an installer. Auth (`--login`) is human-only; the agent only exercises read + download on an already-authenticated session. Downloads land in `~/Games/gog-installers/`; the existing `gog-install` pipeline picks up from there.
---

# gog-library

Drive the user's GOG.com game library through `lgogdownloader` (nonguix channel, in the gaming home profile).  The agent has **list + download** authority only — login stays a human step because the OAuth flow needs a browser.

## When to use

Trigger phrases:
- *"what's in my GOG library?"*, *"list my GOG games"*, *"what do I own on GOG?"*
- *"install / download / get <game> from GOG"*, *"pull down the <game> installer"*
- *"update my <game> installer"*
- *"do I own <game> on GOG?"*
- After a user finishes downloading an installer via GOG's website and asks you to continue with packaging — defer to the existing `gog-install` flow (see **Packaging handoff** below).

Do NOT use this skill for:
- **Steam / Epic / itch** games — `lgogdownloader` is GOG-only.
- **Running** an installed game — that's `~/Games/<Game>/<Binary>` launched directly, or the Guix launcher if it's already packaged in `games.scm`.
- Browsing the GOG storefront / buying games — that's the user in a browser.

## Precondition: auth state

Always check login status **before** trying to list or download:

```bash
lgogdownloader --check-login-status
```

Exit codes:
- `0` + `API: Login successful / HTTP: Login successful` → proceed.
- Non-zero or "Login failed" → **stop and ask the user to run `lgogdownloader --login` themselves** in their terminal.  Don't try to script it — the browser-login flow needs a human paste-back of the GOG redirect URL.

## Listing the library

```bash
# Human-readable (shown to the user in chat)
lgogdownloader --list games

# Machine-parseable — pipe through jq when filtering
lgogdownloader --list json
```

The `json` output gives one big array of game objects with fields like `gamename` (the slug lgogdownloader uses internally — this is what you pass to `--game`), `title` (display name), `platform`, and `tags`.  Use `jq` to filter:

```bash
# Is "torchlight" in my library?
lgogdownloader --list json | jq -r '.games[] | select(.title | test("torchlight"; "i")) | "\(.gamename)\t\(.title)"'
```

When reporting the library to the user, summarize — e.g. "You own 47 games on GOG, including X, Y, Z".  Don't paste the whole list unless explicitly asked.

## Downloading an installer

```bash
# Download Linux installers only, into ~/Games/gog-installers/<gamename>/
mkdir -p ~/Games/gog-installers
lgogdownloader --download \
  --game "^<gamename>$" \
  --platform 4 \
  --directory ~/Games/gog-installers
```

Key flags:
- `--game` takes a **Perl regex** against the slug.  Use `^<name>$` to pin to a single game; a bare `<name>` may match multiple titles.
- `--platform`: `1` = Windows, `2` = macOS, `4` = Linux, `7` = all.  Default to **`4` (Linux)** for this setup.  If the game has no Linux installer, `lgogdownloader` will say so — offer to download the Windows installer with `--platform 1` for Wine/Proton use *only if the user explicitly asks*.
- `--language 1` restricts to English; omit to keep GOG's default (usually all available).
- `--directory` — always `~/Games/gog-installers`.  Do not download to `~/pks/library/` (that's the PKS research library, not games).

### After download

lgogdownloader lays out files as:

```
~/Games/gog-installers/<gamename>/
├── <slug>_linux_en_<version>.sh     # the main installer (what gog-install wants)
├── <slug>_linux_en_<version>.sh.xml # signature/metadata
└── extras/                          # optional PDFs, soundtracks, etc.
```

The main `.sh` file is what `gog-install` consumes.  Point the user (or run directly) at it:

```bash
gog-install ~/Games/gog-installers/<gamename>/<slug>_linux_en_<version>.sh
```

## Packaging handoff

Once `gog-install` finishes, the installed game lives under `~/Games/<DisplayName>/` (the mojosetup installer ignores `--directory`; it uses its own default).  From there follow the flow documented in `entelequia/home/profiles/gaming.scm`:

1. `patchelf --set-interpreter $(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2) <GameBinary>` (Tier 1/2 only).
2. `ldd <GameBinary> | grep "not found"` — pick the tier and library set.
3. Add `(define-public gog-<slug> ...)` to `entelequia/packages/games.scm`.
4. Add `gog-<slug>` to `(gaming-home-packages)` in `entelequia/home/profiles/gaming.scm`.
5. `guix home reconfigure` — launcher + `.desktop` entry appear in the app menu.

The memory file `MEMORY.md` records tier/inputs/quirks for every game already packaged — read those before guessing at a tier for a new one.

## Updating existing installers

```bash
# Re-download only games whose version changed since last download
lgogdownloader --update-check --download --platform 4 \
  --directory ~/Games/gog-installers
```

Update is safe — `lgogdownloader` dedupes by checksum.  Offer this when the user says "update my GOG installers" or after GOG announces a patch.

## Bulk caveats

GOG's download API is not officially public; `lgogdownloader` is a community client and can be rate-limited or throttled.  Do **not** download the entire library in one run without the user asking.  For bulk, cap at 3-5 games per session and show a running status.

## Hard rules

1. **Never run `lgogdownloader --login`, `--gui-login`, or `--browser-login`.**  Enforced by the denylist in `~/.claude/settings.json`.  The OAuth paste-back step is a human keystroke — same category as mail `C-c C-c` or `ntnu-vpn-up`.
2. **Never read `~/.config/lgogdownloader/**`**  — the token file lives there.  Denylisted.
3. **Never write to `~/.config/lgogdownloader/**`**  — config edits stay with the user.  Denylisted.
4. Downloads go to `~/Games/gog-installers/` only.  Do not invent other paths.
5. When GOG account info, email, or any personal field appears in `lgogdownloader` output, strip it before pasting into chat.  The library itself is fine to summarize; the account identity is not.
6. If the user asks to *remove* a game from their GOG account or cancel an order — that's a browser-only workflow on gog.com.  Don't try to script it.

## Quick reference

```bash
lgogdownloader --check-login-status                                   # "am I logged in?"
lgogdownloader --list games                                           # owned titles (human)
lgogdownloader --list json | jq '.games | length'                     # count owned
lgogdownloader --list json | jq -r '.games[].gamename' | grep -i foo  # find slug for "foo"
lgogdownloader --download --game "^<slug>$" --platform 4 \            # Linux installer
  --directory ~/Games/gog-installers
gog-install ~/Games/gog-installers/<slug>/<slug>_linux_en_*.sh        # run installer
```
