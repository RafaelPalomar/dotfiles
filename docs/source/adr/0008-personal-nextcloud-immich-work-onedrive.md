# 0008. Storage split: NextCloud + Immich for personal/family, OneDrive for work

- **Status:** Accepted (OneDrive arm conditional on a per-account Conditional-Access probe)
- **Date:** 2026-06-02
- **Deciders:** Rafael Palomar
- **PR / commit:** *(filled in on merge)*

## Context

The family needs a self-hosted backbone that **non-technical members (Maria,
Leandro 10, Adrian 8) actually use** — i.e. polished **web + iOS/Android apps**
for calendar, files, tasks, photos — and that exposes **agent-usable APIs** so
the household agent (Mary Poppins) can read/write calendar (CalDAV), files
(WebDAV), and a task board (REST). Separately, **work** material (UiO/OUS) must
stay cleanly on the work side of the home/work boundary. NextCloud is already
live on lovelace (NC 33.0.3, drake-karat tailnet). An options-vs-critic
comparison (`arch-confront-backbone` workflow) weighed NextCloud-as-backbone
against a composed best-of-breed stack and against a hybrid, with family
web/mobile UX weighted highest; a separate web-grounded study
(`research-uio-onedrive-linux`) established UiO's OneDrive posture and the
compliance limits.

## Decision

Split storage by **domain, service, and machine identity**:

- **Personal / family → NextCloud (lovelace) + Immich.** NextCloud is the hub —
  Files, Calendar, Deck (tasks), Notes, Talk — one login per family member
  (`maria`, `rafael`, `leandro`, `adrian`, kids' folders scoped), and the
  single unified agent surface (WebDAV + CalDAV + Deck-REST behind one
  `mary-poppins` app-password). Add **exactly one** best-of-breed service —
  **Immich**, on a dedicated drake-karat node (guix-container + ts-sidecar +
  SOPS) — for **photos**, the one surface where best-of-breed decisively beats
  NextCloud on mobile (free polished iOS+Android apps, silent auto-backup).
  Tasks stay on **Deck** (no Vikunja). No Seafile, Radicale/Baïkal, SOGo,
  Joplin, or Syncthing.
- **Work → OneDrive (UiO M365), green-class only.** Work files sync on
  **work-role machines (curie/einstein)** via `rclone` (or the web UI), under
  work identity, **never on baroja** and **never mounted into / indexed by the
  personal PKS** (`~/pks`). Conditional on the per-account CA probe (see
  Conformance) and the green-only compliance red line (ADR context below).

## Alternatives considered

- **Composed best-of-breed (Syncthing + Seafile + Vikunja + Radicale + …).**
  Rejected: Syncthing has **no family-facing app** (Android app discontinued
  2024, no iOS app, no file browser — it is sync plumbing); the stack imposes
  2–3 logins per non-technical member; Vikunja's mobile app is alpha and can
  corrupt its backend; Seafile/Radicale are redundant next to a live NextCloud.
  It wins only on photos — which Option C already takes.
- **Keep work inside NextCloud (the earlier "same account, separate folder").**
  Superseded: moving work to OneDrive removes work from NextCloud entirely, so
  there is no work/personal split to engineer inside NC and no shared-credential
  / fragile-selective-sync risk. Separation is now structural (service +
  identity + machine), matching the two-PKS-roots intent (PKS `20260507T100903`).
- **NextCloud-only (no Immich).** Lowest maintenance, but leaves the one felt
  mobile gap — phone photos (NC Memories has no native iOS app, PWA timeline) —
  on the table for an Android-leaning household that fills photos daily.

## Consequences

- One **extra service** (Immich) to operate; bounded to photos and fully
  reversible (delete the node → re-enable NC camera-upload + Memories).
- **Camera-upload cutover footgun:** designate Immich as the camera-roll target
  and stand down NC auto-upload, or two competing photo backups result.
- **NextCloud is a PHP monolith → single blast radius + major-upgrade
  fragility** (Deck schema migrations, Talk's HPB). Mitigate with per-surface
  scoped app-passwords, version pinning, and snapshot-before-upgrade.
- **OneDrive is green-only on an unmanaged Linux box;** yellow/internal is
  presumptively disallowed and red/patient/special-category health data is
  legally prohibited (TSD-only, GDPR Art. 9 / Norwegian health law). The bridge
  is class-gated at source, never a whole-drive auto-pull, and off the PKS.
- Retires the Phase-2 NextCloud-account dilemma; the two PKS roots are now
  NextCloud (personal) + OneDrive (work).

## Conformance

- The work OneDrive client runs only on a `(home-role 'work)` machine (ADR-0005)
  and the synced tree is never under `~/pks` nor indexed by `denotecli`.
- Before any work sync: run the **AADSTS login probe** (`rclone config` →
  onedrive Business → interactive browser OAuth + Feide/Entra MFA, **not**
  device-code) on curie/einstein and record success / the AADSTS code.
- Immich is pinned to a known-good **2.5.x (2.5.6)**: the 2026 privesc
  (CVE-2026-23896) was fixed *in* 2.5.0, so **≥2.5.0 is the safe band** —
  re-confirm the current CVE posture at deploy. Stood up via the established
  guix-container + ts-sidecar + SOPS pattern; its API key (if the agent gets
  photo access) is separate and scoped. Host: a **dedicated node** (recommended;
  Immich ships its own Postgres-14+VectorChord and its ML backfill wants 2–4 GB)
  or **staged on edison** (15 GB; cap ML concurrency, migrate later) — **never
  lovelace** (7.5 GB, already saturated by NextCloud + ~10 containers).

## References

ADR-0005 (home/work role), ADR-0006 (agent identities); PKS `20260507T100903`
(two-PKS-roots), PKS `20260421T194720`;
`~/.claude/plans/family-agentic-architecture.md` (§2.3, §3) and the
`arch-confront-backbone` + `research-uio-onedrive-linux` workflow findings.
