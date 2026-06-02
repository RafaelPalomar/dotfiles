# 0008. Storage split: NextCloud (native photos) for personal/family, OneDrive for work

- **Status:** Accepted (OneDrive arm conditional on a per-account Conditional-Access probe)
- **Date:** 2026-06-02 *(revised same day — Immich dropped from the baseline; see "Revision" below)*
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

Split storage by **domain, service, and machine identity** — with **one
backbone, one app, one login** per family member on the personal side:

- **Personal / family → NextCloud (lovelace), photos handled natively.**
  NextCloud is the hub — Files, Calendar, Deck (tasks), Notes, Talk — one login
  per family member (`maria`, `rafael`, `leandro`, `adrian`, kids' folders
  scoped), and the single unified agent surface (WebDAV + CalDAV + Deck-REST
  behind one `mary-poppins` app-password). **Phone photos ride the same
  backbone:** the NextCloud mobile app's **auto-upload** pushes the camera roll
  into NextCloud Files, and the **Memories** app presents the timeline / albums
  / map / video over those files. **Recognize** (server-side ML: faces, objects,
  scenes) is added *later and off-peak* for search — optional, not on the
  critical path. **No Immich, no second node, no second app, no second auth.**
  Tasks stay on **Deck** (no Vikunja). No Seafile, Radicale/Baïkal, SOGo,
  Joplin, or Syncthing.
- **Work → OneDrive (UiO M365), green-class only.** Work files sync on
  **work-role machines (curie/einstein)** via `rclone` (or the web UI), under
  work identity, **never on baroja** and **never mounted into / indexed by the
  personal PKS** (`~/pks`). Conditional on the per-account CA probe (see
  Conformance) and the green-only compliance red line (Consequences below).

## Revision (2026-06-02)

The first draft of this ADR (same day) added **Immich** on a dedicated node as
"the one best-of-breed mobile win" for photos. Dropped before merge: NextCloud's
own **auto-upload + Memories** already closes the capture → sync → gallery loop
on the backbone we committed to, so Immich re-introduced exactly the cost we
rejected Syncthing for — a second service, second mobile app, second login,
second backup + update + CVE surface (CVE-2026-23896), and a dedicated node.
Immich is **demoted to a documented fallback** (see Alternatives): revisit only
if Memories proves inadequate at family scale.

## Alternatives considered

- **Add Immich on a dedicated node for photos (the earlier draft).** Deferred,
  not baseline. Immich genuinely beats NextCloud-native on three axes — a
  slicker native mobile app (Google-Photos-grade background upload + scrubbing),
  faster/semantic ML (CLIP search, stronger face rec than Recognize), and scale
  (tens of thousands of photos). But those are **luxuries, not baseline
  requirements** for this family, and each costs a whole second service + app +
  auth + a dedicated node (Immich ships its own Postgres-14+VectorChord; ML
  backfill wants 2–4 GB — **never lovelace**, which is RAM-bound). Kept as a
  **fallback** only if NextCloud-native photos disappoint at scale.
- **Composed best-of-breed (Syncthing + Seafile + Vikunja + Radicale + …).**
  Rejected: Syncthing has **no family-facing app** (Android app discontinued
  2024, no iOS app, no file browser — it is sync plumbing); the stack imposes
  2–3 logins per non-technical member; Vikunja's mobile app is alpha and can
  corrupt its backend; Seafile/Radicale are redundant next to a live NextCloud.
- **Keep work inside NextCloud (the earlier "same account, separate folder").**
  Superseded: moving work to OneDrive removes work from NextCloud entirely, so
  there is no work/personal split to engineer inside NC and no shared-credential
  / fragile-selective-sync risk. Separation is now structural (service +
  identity + machine), matching the two-PKS-roots intent (PKS `20260507T100903`).

## Consequences

- **Lowest-maintenance personal side:** one backbone, one mobile app, one login —
  no extra photo service to operate, secure, back up, or upgrade.
- **Camera-upload is single-source:** NextCloud auto-upload is the only
  camera-roll target, so there is no competing-backup footgun.
- **Recognize ML lands on lovelace** (7.5 GB, ~10 containers, just freed ~1.5 GB
  by retiring Habitica). Face/object indexing is CPU/RAM-heavy → enable it
  *after* the timeline is live, schedule the initial backfill **off-peak**, or
  leave faces off. This is the one piece that would have wanted a dedicated node;
  it is optional and deferrable.
- **Mobile background-upload reliability must be validated on real phones**
  (Maria's + the kids') — NextCloud auto-upload is good but historically less
  bulletproof than Immich/Google Photos under Android battery-kill / iOS
  background limits.
- **Photo storage counts against lovelace `/data` + per-user NextCloud quota** —
  a family library is easily 100s of GB; plan disk headroom + quotas. Optional
  `go-vod` transcoder smooths mobile video playback.
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
- Photos: enable the **Memories** app + the NextCloud mobile app's auto-upload;
  validate background upload on the family's actual phones before declaring the
  photo surface done. Defer **Recognize** until the timeline is in use and run
  its first index off-peak (lovelace is RAM-bound). `go-vod` only if mobile video
  playback needs it.
- If NextCloud-native photos prove inadequate at scale, the **Immich fallback**
  stands up via the established guix-container + ts-sidecar + SOPS pattern on a
  dedicated node (never lovelace), pinned to a known-good ≥2.5.0 build
  (CVE-2026-23896 was fixed *in* 2.5.0; re-confirm CVE posture at deploy).

## References

ADR-0005 (home/work role), ADR-0006 (agent identities); PKS `20260507T100903`
(two-PKS-roots), PKS `20260421T194720`;
`~/.claude/plans/family-agentic-architecture.md` (§2.3, §3) and the
`arch-confront-backbone` + `research-uio-onedrive-linux` workflow findings.
