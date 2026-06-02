# Hermes + Mattermost on edison — deploy runbook

Tiered family assistant: a self-hosted Mattermost chat fronting three Hermes
gateway tiers. This branch (`hermes-edison-deploy`) adds it to edison; full
design rationale is in `~/.claude/plans/hermes-final-edison-deploy.md`.

## Tiers (3 distinct power envelopes)

| Tier | Brain (OpenRouter) | Executor | LAN? | Runtime |
|------|--------------------|----------|------|---------|
| `hermes-tutor` (kids) | `google/gemini-3.1-flash-lite` | `openai/gpt-5.4-nano` | no | rootless Podman |
| `hermes-household` | `google/gemini-3-pro-preview` | `mistralai/mistral-medium-3-5` | no | rootless Podman |
| `hermes-ops` (parents) | `anthropic/claude-sonnet-4.6` | `anthropic/claude-haiku-4.5` | **yes** | **guix container** |

All models via one OpenRouter gateway (metered, ~$57/mo). Per-tier
`provider_routing: data_collection: deny` + Western-only allowlists. `ops` runs
as `guix shell --container` sharing `/gnu/store` + `/var/guix` so the agent can
`guix shell` admin tools (capability precedent: ivs ADR-0007). tutor/household
are store-free Podman containers. Hermes is outbound-only (no inbound port).

## Deploy order

> Build the image first; bring up Phase 1; bootstrap Mattermost by hand; then
> fill secrets and deploy the Hermes tiers. The Hermes containers crash-loop
> until their bot tokens exist — expected.

1. **Build the image on edison** (manifest + script live on `guix-hermes` branch
   `oci-pack-image`, pushed to OUH-MESHLab):
   ```sh
   # on edison, in a guix-hermes checkout @ oci-pack-image:
   ./scripts/build-hermes-image.sh        # → localhost/hermes:e93f670…
   ```
2. **Seed sops** — add to `sops/edison.yaml`, then re-encrypt (`mattermost/*`
   first; bot tokens + IDs come after step 4):
   ```yaml
   mattermost: { db_password: …, admin_password: … }
   hermes-tutor:     { env: { MATTERMOST_URL: http://192.168.88.14:8065, OPENROUTER_API_KEY: …, OPENAI_API_KEY: … (moderation only), MATTERMOST_TOKEN: …, MATTERMOST_ALLOWED_USERS: <kid IDs>, MATTERMOST_ALLOWED_CHANNELS: <#learn ID> } }
   hermes-household: { env: { MATTERMOST_URL: …, OPENROUTER_API_KEY: …, MATTERMOST_TOKEN: …, MATTERMOST_ALLOWED_USERS: <family IDs>, MATTERMOST_ALLOWED_CHANNELS: <#household ID> } }
   hermes-ops:       { env: { MATTERMOST_URL: …, OPENROUTER_API_KEY: …, MATTERMOST_TOKEN: …, MATTERMOST_ALLOWED_USERS: <parent IDs>, MATTERMOST_ALLOWED_CHANNELS: <#ops ID> } }
   ```
   Three distinct OpenRouter keys; set per-key spend caps (ops $35 / household
   $50 / tutor $20, monthly) + account no-train opt-out in the OpenRouter UI.
3. **Deploy** Mattermost + Postgres (tiers will crash-loop — fine):
   ```sh
   cd ~/.dotfiles && git merge hermes-edison-deploy   # or deploy from the worktree
   ./scripts/deploy.sh edison
   ```
4. **Bootstrap Mattermost** (interactive, https://mattermost.drake-karat.ts.net
   or http://192.168.88.14:8065): create admin → team → 3 **bot accounts**
   (`hermes-tutor/household/ops`, capture tokens) → channels `#learn`/`#household`/`#ops`
   (capture 26-char IDs) → add each bot to its channel → collect each family
   member's 26-char **User ID**.
5. **Fill real tokens + IDs** into sops *and* substitute the `REPLACE_*_CHANNEL_ID`
   placeholders in the three seeded `config.yaml`s → re-encrypt → re-deploy.
6. **Smoke test** per tier in Mattermost:
   - `#learn` (kid): answers a coding question; an infra ask has no tool / no LAN.
   - `#household`: produces a plan; cannot reach RFC-1918.
   - `#ops` (parent): reads a LAN service; a dangerous command triggers the
     in-channel manual approval; a kid's User ID is denied.

## Verify-at-apply (3 gotchas)

- **`provider_routing` actually honored** on the installed Hermes (bug #5358 can
  silently fall back) — **test before household/ops touch real data**; the
  Western/no-train guarantee depends on it.
- **`guix shell -E`** env-forwarding semantics on edison's guix version (ops
  secret injection in `edison-hermes-ops-service`).
- The 8 OpenRouter slugs resolve; `gemini-3-pro-preview` is *preview* — swap to
  GA when it ships.

## Notes
- `herd status` should show: `mattermost`, `postgres-mattermost`, `ts-mattermost`,
  `hermes-tutor`, `hermes-household`, `hermes-ops`.
- ops is **read/diagnostic only** at launch (no host-mutating tools). Promote to
  `terminal.backend: ssh` against a hardened worker when mutation is wanted.
- Files: `entelequia/system/lib/edison-services.scm`, `entelequia/system/machines/edison.scm`.
