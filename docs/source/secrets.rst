===================================
Key & Secret Management (Canonical)
===================================

.. note::

   This is the **single source of truth** for how secrets and
   cryptographic keys are organised across entelequia.  It supersedes
   the scattered guidance previously split across ``gpg.rst``,
   ``dotfiles/.keys/README.md``, ``SETUP-COMPLETE.md`` and
   ``procedures/MASTER-GUIDE.md``.  Operational how-tos (key
   generation, rotation) live in the ``keys`` CLI (``keys <cmd>
   --help``) and the ADR ledger, not duplicated in prose.

   - **What goes where:** the decision tree below.
   - **Who holds which key:** :doc:`keys-inventory`.
   - **Why each decision:** :doc:`adr/index`.

Why this exists
===============

Historically secrets were spread across five mechanisms (``pass``,
Bitwarden, SOPS, raw GnuPG, ad-hoc SSH/deploy keys) with no written
rule for which to use, four overlapping doc sources, and eight loose
key-generation scripts that were never version-controlled.  The result
was *fragmented, hard to remember, and did not scale* (notably: the
GnuPG-SSH agent offering more keys than a hardened server's
``MaxAuthTries`` allows).

The consolidation does **not** replace the tools — each is justified.
It (a) writes down one decision tree, (b) unifies the entry points
behind a single ``keys`` CLI, and (c) lifts the discipline already
proven in ``~/src/ivs-infrastructure`` (a committed key inventory + an
audit script + ADRs) up to the personal scope.

The decision tree
==================

Two questions, asked in order, place every secret:

.. mermaid::

   flowchart TD
       A["A secret / credential to store"] --> B{"For a HUMAN to use<br/>across devices?<br/>(web/app login, phone, browser)"}
       B -- yes --> BW["<b>Bitwarden</b> (cloud)<br/>logins, TOTP, cards, shared creds"]
       B -- no --> C{"Will the consuming system<br/>be <code>guix deploy</code>'d?"}
       C -- "no (used/operated directly, ad-hoc)" --> P["<b>pass</b><br/>GPG-encrypted, machine-local,<br/>materialised to an envfile"]
       C -- "yes (structured, planned, in git)" --> S{"Personal box<br/>or fleet host?"}
       S -- "personal dotfiles" --> SO["<b>SOPS</b> — operator-only pattern<br/>(recipient = personal key)"]
       S -- "fleet host" --> SH["<b>SOPS</b> — host-side pattern<br/>(recipients = operator + host key)"]

The grey zone — a directly-used box that later graduates to
``guix deploy`` — has one rule: **when the box graduates, its secret
graduates** ``pass`` → SOPS.  That move is a documented one-liner
(``keys`` will grow a helper), not a re-think.

Lane summary
------------

.. list-table::
   :header-rows: 1
   :widths: 14 20 30 36

   * - Lane
     - Tool
     - Trust root
     - Use for
   * - Human
     - Bitwarden (cloud)
     - Bitwarden master password
     - Web/app logins, TOTP, cards, identities, anything typed on a
       phone or autofilled in a browser.
   * - Programmatic, ad-hoc
     - ``pass``
     - personal GnuPG key
     - Secrets consumed by scripts/agents on a box you operate
       directly (e.g. ``hermes/*`` LLM + messaging keys, email OAuth
       refresh tokens).  Materialised into an envfile by
       ``keys sync``.
   * - Programmatic, deployed
     - SOPS (GnuPG)
     - personal key (personal) / operator + host keys (fleet)
     - Secrets for ``guix deploy``'d systems, committed encrypted to
       git.  See the two patterns below.

The two SOPS patterns are **deliberately different**, chosen per
threat model — see
:doc:`the PKS theory notes <adr/index>` and ADR-0002:

operator-only
   The operator workstation holds the only decryption key; ciphertext
   is the only thing in git; plaintext is produced at edit time.
   Appropriate for *personal* infrastructure where the operator is the
   threat model.  This is what ``dotfiles/sops/*.yaml`` uses today.

host-side
   Each host holds its own key and decrypts at activation into a tmpfs
   (``/run/secrets``); plaintext never enters ``/gnu/store``.
   Appropriate for fleets where blast-radius isolation between hosts
   matters.  This is what ``~/src/ivs-infrastructure`` uses.

Identities (trust roots)
========================

.. list-table::
   :header-rows: 1
   :widths: 26 18 24 32

   * - Identity
     - Fingerprint
     - Lives
     - Roots
   * - **Personal master** ``Rafael Palomar <rafael@palomar.no>``
     - ``6513…A72F``
     - master air-gapped on IronKey; ``[S][E][A]`` subkeys for daily
       use
     - git/mail signing, SSH auth, per-deploy SSH subkeys, ``pass``,
       personal dotfiles SOPS (operator-only)
   * - **IVS operator** ``Rafael Palomar <rafpal@ous-hf.no>``
     - ``8EADF28F…``
     - curie ``~/.gnupg``
     - ``~/src/ivs-infrastructure`` SOPS (host-side)
   * - **Per-host SOPS keys**
     - per host
     - each fleet host ``/root/.gnupg``
     - that host's ``/run/secrets``
   * - **Bitwarden master**
     - —
     - cloud + operator memory
     - human vault

.. note::

   **Identity reconciled (2026-05-30).**  The IVS operator key
   ``8EADF28F…`` now carries the primary UID ``Rafael Palomar
   <rafpal@ous-hf.no>`` (the institutional research address); the old
   ``Rafael IVS SOPS <ivs-sops@palomar.no>`` is retained as a secondary
   UID.  The fingerprint is unchanged, so SOPS recipients are
   unaffected.  IVS-repo follow-up: distribute the updated public key
   (``/tmp/ivs-operator-pub.asc``) and update that repo's inventory.

The design is deliberately extensible: a future fourth identity
(e.g. an NTNU role key) is added by the same recipe — a dedicated
key with an institutional UID, registered in the inventory, scoped to
its own SOPS recipients.

The SSH many-keys problem
=========================

A single GnuPG-SSH agent offers **every** enabled ``[A]`` subkey to
every server it connects to.  Once it offers more keys than the
server's ``MaxAuthTries`` (OpenSSH default 6; **3 on the hardened
entelequia sshd**) before the right one, the server rejects the
connection with ``Too many authentication failures``.  With seven auth
subkeys today (one personal + six deploy: monk, lovelace, edison,
hopper, hamming, baroja) this already bites — four are unreachable on a
hardened host unless ordered perfectly in ``sshcontrol``.

**The fix — pin one key per host.**  For each target, the SSH client
config offers exactly one identity:

.. code-block:: text

   Host hamming
       IdentityAgent  /run/user/1000/gnupg/S.gpg-agent.ssh
       IdentityFile   ~/.keys/ssh/deploy/hamming.pub
       IdentitiesOnly yes

``IdentitiesOnly yes`` plus a pinned **public** key file (the private
half stays in the agent) makes the client present only that key —
never tripping ``MaxAuthTries``, regardless of how many the agent
holds.  This is the scalability unlock: the hundredth deploy key still
costs one offer per host.  ``sshcontrol`` curation (enable only what is
needed) stays as defence-in-depth, but the "order most-used first"
hack becomes unnecessary.

This config is rendered declaratively via Guix home (planned Phase 2);
public deploy keys are emitted by ``keys deploy pubkey <name>`` and
stored under ``~/.keys/ssh/deploy/`` (public → safe to version).
Rationale: ADR-0003 (this repo).

Tooling — the ``keys`` CLI
==========================

One entry point replaces the eight loose ``gpg-*.sh`` scripts plus
``manage-deploy-keys.sh`` and ``hermes-pass-sync``:

.. list-table::
   :header-rows: 1
   :widths: 34 66

   * - Command
     - Does
   * - ``keys ceremony add-subkey|rotate|expire``
     - The scripted **IronKey** ceremony: mount → tmpfs ``GNUPGHOME`` →
       import master → add/rotate subkey → strip secret master →
       re-import working keyring → wipe tmpfs → unmount.  Keeps the
       air-gap (master bytes never touch persistent disk); removes the
       manual fumbling.  See ADR-0004.
   * - ``keys deploy add|list|enable|disable|pubkey``
     - Per-deployment ``[A]`` subkey management (absorbs
       ``manage-deploy-keys.sh``).
   * - ``keys sync hermes``
     - Materialise ``pass`` secrets into ``~/.hermes/secrets.env``
       (absorbs ``hermes-pass-sync``).
   * - ``keys host-sops <host>``
     - Generate a per-host SOPS GnuPG key on a fleet host (ports the
       IVS ``new-host-sops-key.sh``).
   * - ``keys audit [--static]``
     - Reconcile :doc:`keys-inventory` against live keyrings; exits
       non-zero on drift. ``--static`` is the keyring-free lint
       (inventory well-formed + every ``.sops.yaml`` recipient is
       inventoried). Wired as a gate: ``.githooks/pre-commit`` (full
       on operator machines, static fallback elsewhere) and
       ``.github/workflows/keys-audit.yml`` (static). Enable the hook
       once per clone: ``git config core.hooksPath .githooks``.
   * - ``keys inventory``
     - Show / open the key inventory.
   * - ``keys manifest [--identity X]``
     - Print the reproducible identity hash of a key (sorted
       fingerprints + keygrips + caps + dates).
   * - ``keys backup verify --file PATH``
     - Confirm a secret-key backup is intact: storage checksum + the
       secret primary is present + identity manifest matches.
   * - ``keys escrow <identity> --to PATH.gpg``
     - Export + encrypt a secret key (asymmetric to the personal
       master by default, or ``--method symmetric``), record both
       checksums, and auto-verify. Safe by default (``--yes`` performs).
   * - ``keys paperkey [--identity X]``
     - Printable offline backup of a key's secret material (via
       ``paperkey``); for the air-gapped master, inside an IronKey/tmpfs
       session. Safe by default (``--yes`` performs).

.. note::

   **Status (Phase 1, done):** ``keys`` is implemented at
   ``dotfiles/.local/bin/keys`` and deploys via
   ``home-dotfiles-service-type``.  ``deploy`` and ``sync`` delegate to
   the proven ``manage-deploy-keys.sh`` / ``hermes-pass-sync`` (still
   directly invocable); ``audit``, ``host-sops``, ``inventory`` and
   ``ceremony`` are implemented in ``keys`` itself.

   ``keys ceremony`` is **safe-by-default**: it prints the plan and only
   mutates with ``--yes``, and enforces a hard rail that the master is
   imported solely into a tmpfs ``GNUPGHOME`` (never persistent disk).
   It is untested against real IronKey hardware — **review the dry-run
   plan before the first** ``--yes`` **run**.

   The eight loose ``~/.local/bin/gpg-*.sh`` bootstrap scripts are
   superseded by ``keys ceremony`` for ongoing add/rotate; retire them
   once you are comfortable (they are untracked one-time-generation
   scripts).

Escrow & disaster recovery
===========================

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Asset
     - Recovery posture
   * - Personal master ``6513…A72F``
     - Air-gapped on IronKey (triple-encrypted). Paper escrow via
       ``keys paperkey --identity personal --yes`` (IronKey/tmpfs
       session) → **print, store in the safe, shred the file**. Action
       pending (tooling ready).
   * - Revocation certificate
     - Printed, stored in a safe separate from the IronKey.
   * - Daily subkeys
     - Regenerable from the master via ``keys ceremony``; encrypted
       backups under ``~/.keys/gpg/subkeys-backup/``.
   * - IVS operator key ``8EADF28F…``
     - ``keys escrow ivs --to <path>.gpg --yes`` — exports + encrypts to
       the personal master (recovery chains through the offline master)
       + auto-verifies. Place the output on the IronKey or in the IVS
       repo (**not** this personal repo). Action pending (tooling ready).
   * - Per-host SOPS keys
     - Not escrowed by design — regenerate on host loss and
       ``sops updatekeys`` (defence-in-depth).
   * - Bitwarden vault
     - Cloud-synced; export an encrypted backup periodically.

**Verifying a backup is intact.** A naïve ``sha256`` of an exported
secret key is *not* reproducible: GnuPG re-applies S2K protection with a
fresh random salt (and IV) on every export, so the bytes differ each
time even for an identical key. ``keys`` therefore records **two**
checksums beside each IronKey master backup and checks both via
``keys backup verify --file <path>``:

- ``master-key.asc.sha256`` — *storage* checksum of the exact bytes
  written, recorded at write time; detects bit-rot/corruption of *that*
  file (not comparable across re-exports — and isn't meant to be).
- ``master-key.manifest.sha256`` — a *reproducible identity* hash: the
  sorted fingerprints + keygrips + caps + dates of the primary and every
  subkey (``keys manifest``). Deterministic, so it confirms the backup
  holds the right key material, comparable across re-exports/machines.

``keys backup verify`` imports the backup into a tmpfs keyring, asserts
the secret primary is actually present, and compares both checksums.

Cases covered
=============

Every secret/key case and where it lands:

.. list-table::
   :header-rows: 1
   :widths: 6 50 44

   * - #
     - Case
     - Lands in
   * - 1
     - Git commit / tag signing
     - personal ``[S]`` subkey
   * - 2
     - Encrypted mail
     - personal ``[E]`` subkey
   * - 3
     - Email OAuth refresh tokens
     - ``pass`` (``email/*.gpg``)
   * - 4
     - Email OAuth client id / secret
     - dotfiles SOPS (``rafael.yaml``)
   * - 5
     - SSH → GitHub
     - personal ``[A]`` subkey
   * - 6
     - SSH → own machines
     - personal ``[A]`` subkey, pinned per-host
   * - 7
     - ``guix deploy`` → fleet host
     - per-deploy ``[A]`` subkey, pinned per-host
   * - 8
     - Agent/script API keys (LLM, messaging)
     - ``pass`` → ``~/.hermes/secrets.env`` via ``keys sync hermes``
   * - 9
     - Personal deployed-service secrets
     - dotfiles SOPS (operator-only)
   * - 10
     - Fleet service secrets
     - ``ivs-infrastructure`` SOPS (host-side)
   * - 11
     - Human web/app logins, mobile, TOTP
     - Bitwarden
   * - 12
     - Nextcloud OAuth via Secret Service
     - ``pass-secret-service`` (bridges ``pass``)
   * - 13
     - Disk / IronKey hardware passphrases
     - Bitwarden (human recall) + paper in safe
   * - 14
     - Disaster recovery
     - IronKey + ``paperkey`` + printed revocation cert

See also
========

- :doc:`keys-inventory` — who holds which key (source of truth).
- :doc:`adr/index` — the decision ledger (why each choice was made).
- :doc:`gpg` — *(legacy; retained for the detailed GnuPG command
  reference until folded into the* ``keys`` *CLI help).*
- :doc:`bitwarden-rofi` — desktop ``rofi-rbw`` autotype wiring.
- ``~/src/ivs-infrastructure`` — ``Docs/adr/0003`` (SOPS),
  ``Docs/adr/0006`` (key inventory & audit), ``Docs/keys-inventory.md``
  — the fleet-side exemplar this consolidation mirrors.
