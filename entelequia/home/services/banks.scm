(define-module (entelequia home services banks)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)                 ; banks-cli wrapper package
  #:use-module ((guix licenses) #:prefix license:)         ; license: (banks-cli)
  #:use-module (guix profiles)                             ; profile, packages->manifest
  #:use-module (mr-banks agent)                            ; make-banks(-launcher)
  #:use-module (mr-banks bridge)                           ; banks-bridge (Mattermost daemon)
  #:use-module (mr-banks ops)                              ; banks-watchdog, banks-digest
  #:use-module (mr-banks ingest)                           ; banks-ingest, mr-banks-label, banks-file
  #:use-module (guix-agentic agents core)                  ; agent->package, agent->manifest-entries
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:use-module (gnu home services shepherd)                ; home-shepherd-service-type
  #:use-module (gnu home services mcron)                   ; home-mcron-service-type
  #:use-module (gnu services shepherd)                     ; shepherd-service, forkexec
  #:use-module (gnu packages bash)                         ; bash-minimal (/bin/sh)
  #:export (banks-home-service))

;;; banks — Mr. Banks, the household finance agent (PERSONAL domain, DIRECT).
;;;
;;; Belongs on the family server (edison).  He runs the premium Claude model tier
;;; (via OpenRouter, like the rest of the fleet — the trust distinction is the
;;; MODEL, not the provider) and talks to the family through his OWN `mr-banks'
;;; bot — Poppins never relays his figures (arch-review B1).  The wrapper injects,
;;; at launch, the secrets the L1 container scrubs:
;;;   OPENROUTER_API_KEY <- OPENROUTER-ENV-FILE  (dotenv; reuses the household key
;;;                                               Poppins reads, no dedicated sops
;;;                                               key — repoint to a Banks-owned
;;;                                               key in a later pass)
;;; and sets the non-secret MR_BANKS_LEDGER + MR_BANKS_HOUSEHOLD (absolute paths to
;;; the read-only ledger and household roster the sandbox --expose's).  agent.scm's
;;; sandbox preserves all three.
;;;
;;; Deploy prereqs (system side): the household dotenv sops-secret
;;; (hermes-household/env) already present for Poppins; the beancount ledger +
;;; household.md present at LEDGER-ROOT, owner-segregated + read-only
;;; (arch-review B5); the `mr-banks' bot provisioned with its token/channel in
;;; MM-FRAGMENT.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

(define %default-ledger-root "/var/lib/mr-banks/ledger")
(define %default-household-file "/var/lib/mr-banks/household.md")
;;; The PRIVATE ops folder: importers, reviewed rules, and the recategorize
;;; tool.  Placed out of band like the ledger (it encodes this household's bank
;;; formats and merchant names, so it stays out of the public channel).  Only
;;; the bridge reads it — the agent never sees it.
(define %default-ops-root "/var/lib/mr-banks/ops")
;;; Filed papers (invoices, bills, payslips).  Written by the bridge via
;;; banks-file; --expose'd READ-ONLY into the agent so he can read them.
(define %default-docs-root "/var/lib/mr-banks/documents")

(define (banks-wrapper openrouter-env-file ledger-root household-file docs-root)
  "An executable `banks' that injects OPENROUTER_API_KEY + MR_BANKS_LEDGER +
MR_BANKS_HOUSEHOLD and execs the banks launcher with its tool profile on
GUIX_ENVIRONMENT (so the `mr-banks' CLI + the skill land on PATH inside the L1
container).  OPENROUTER_API_KEY is read from the household dotenv
OPENROUTER-ENV-FILE — the same key Poppins reuses (/run/secrets/hermes-household/env)
— rather than a Banks-dedicated sops key; repoint to a Banks-owned key later."
  (let* ((banks-agent (make-banks #:ledger-root ledger-root
                                  #:household-file household-file
                                  #:documents-root docs-root))
         (banks-launcher (agent->package banks-agent))
         (tool-profile (profile (content (packages->manifest
                                          (agent->manifest-entries banks-agent))))))
    (program-file
     "banks"
     #~(begin
         (use-modules (ice-9 rdelim) (srfi srfi-13))
         ;; Extract VAR=value from a dotenv file (KEY=VALUE lines), stripping
         ;; surrounding quotes — mirrors poppins.scm so Banks reuses the same
         ;; household OpenRouter key without a separate sops secret.
         (define (read-dotenv f var)
           (and (file-exists? f)
                (call-with-input-file f
                  (lambda (p)
                    (let ((pfx (string-append var "=")))
                      (let loop ()
                        (let ((line (read-line p)))
                          (cond ((eof-object? line) #f)
                                ((string-prefix? pfx line)
                                 (let* ((v (substring line (string-length pfx)))
                                        (n (string-length v)))
                                   (if (and (>= n 2) (memv (string-ref v 0) '(#\" #\')))
                                       (substring v 1 (- n 1))
                                       v)))
                                (else (loop))))))))))
         (let ((k (read-dotenv #$openrouter-env-file "OPENROUTER_API_KEY")))
           (when k (setenv "OPENROUTER_API_KEY" k)))
         ;; Absolute path to the read-only ledger root the sandbox --expose's;
         ;; mrbanks.py resolves MR_BANKS_LEDGER via os.path.abspath, so this is safe.
         (setenv "MR_BANKS_LEDGER" (string-append #$ledger-root "/main.beancount"))
         ;; Household roster (names/ages) — private, --expose'd read-only; the
         ;; persona reads member names from this path (never baked into the channel).
         (setenv "MR_BANKS_HOUSEHOLD" #$household-file)
         ;; Where the household's filed papers live (read-only in the sandbox).
         (setenv "MR_BANKS_DOCS" #$docs-root)
         (unless (getenv "GUIX_ENVIRONMENT")
           (setenv "GUIX_ENVIRONMENT" #$tool-profile))
         ;; The launcher symlinks skills into the config dir from this search path.
         (setenv "GUIX_AGENTIC_PI_SKILL_PATH"
                 (string-append #$tool-profile "/share/pi/skills"))
         (apply execl #$(file-append banks-launcher "/bin/banks")
                "banks" (cdr (command-line)))))))

(define (banks-cli openrouter-env-file ledger-root household-file docs-root)
  (package
    (name "banks-cli")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(banks-wrapper openrouter-env-file ledger-root
                                           household-file docs-root)
                          (string-append #$output "/bin/banks"))
               (chmod (string-append #$output "/bin/banks") #o755))))
    (synopsis "Mr. Banks launcher wrapper (injects OPENROUTER_API_KEY + MR_BANKS_LEDGER)")
    (description "Wrapper that injects the OpenRouter key (from a sops-decrypted
file) plus the ledger + household-roster paths, then execs the banks launcher.")
    (home-page "https://github.com/RafaelPalomar/mr-banks")
    (license license:gpl3+)))

;;; --- the Mattermost bridge (chat surface) ----------------------------------

(define %mm-fragment "/var/lib/mattermost-provision/mr-banks.env")

;; The bridge profile carries the WRITE PATH too: banks-ingest (a statement
;; dropped in the channel) and mr-banks-label (a rule the household dictates in
;; chat).  Both run here, as the bridge's user — never inside the agent's
;; sandbox, which keeps the agent's own view of the ledger read-only.
(define banks-bridge-profile
  (profile (content (packages->manifest
                     (list banks-bridge banks-ingest mr-banks-label banks-file)))))

(define (banks-bridge-start-script mm-fragment mm-origin ops-root ledger-root docs-root)
  (mixed-text-file "banks-bridge-start"
    "#!/bin/sh\nset -e\n"
    "FRAG=\"" mm-fragment "\"\n"
    "[ -f \"$FRAG\" ] || { echo 'banks-bridge: waiting for MM fragment' >&2; sleep 10; exit 1; }\n"
    "set -a\n. \"$FRAG\"\nset +a\n"
    "export MATTERMOST_ORIGIN=\"" mm-origin "\"\n"
    ;; Where the importers, rules and ledger live for the write path.
    "export MR_BANKS_OPS=\"" ops-root "\"\n"
    "export MR_BANKS_LEDGER_DIR=\"" ledger-root "\"\n"
    "export MR_BANKS_DOCS=\"" docs-root "\"\n"
    ;; His face, shipped with the bridge and re-applied on every start — so a
    ;; recreated bot (renaming one creates a new account) comes back with it.
    "export BANKS_AVATAR=\"" (file-append banks-bridge "/share/mr-banks/avatar.png") "\"\n"
    ". " (file-append banks-bridge-profile "/etc/profile") "\n"
    ;; banks-ingest/mr-banks-label come from the profile sourced above; the
    ;; `banks' wrapper itself is in the home profile.
    "export PATH=\"" (file-append banks-bridge-profile "/bin")
    ":$HOME/.guix-home/profile/bin:$PATH\"\n"
    "exec " (file-append banks-bridge-profile "/bin/banks-bridge") "\n"))

(define (banks-bridge-shepherd-service mm-fragment mm-origin ops-root ledger-root docs-root)
  (list
   (shepherd-service
    (documentation "Mr. Banks Mattermost bridge (mr-banks bot -> banks -p)")
    (provision '(banks-bridge))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh")
                    #$(banks-bridge-start-script mm-fragment mm-origin
                                                 ops-root ledger-root docs-root))
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/banks-bridge.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    ;; Survive the boot race with Mattermost.  The bridge exits non-zero while
    ;; the MM fragment is missing or the server is not yet listening; with
    ;; shepherd's stock limit (5 respawns in 5 s) that fast loop DISABLES the
    ;; service, and Banks stays silently dead until a human notices (it did,
    ;; for four days, after the 2026-08-12 reboot).  A 15 s delay stretches any
    ;; loop far outside the window below, so the limit only ever catches a
    ;; genuinely broken binary.
    (respawn-delay 15)
    (respawn-limit #~'(10 . 60)))))

;;; --- liveness watch --------------------------------------------------------
;;;
;;; Banks answers when asked, so "dead" and "nobody asked this week" look the
;;; same from the outside — the 2026-08-12 boot race went unnoticed for four
;;; days on exactly that ambiguity.  The watchdog reads the bridge's heartbeat
;;; file, restarts the service if it can, and DMs the finance owner (never
;;; #finance: an ops alert there is noise to the rest of the household).

(define banks-watchdog-profile
  (profile (content (packages->manifest (list banks-watchdog)))))

(define (banks-watchdog-start-script mm-fragment ledger-root alert-username)
  (mixed-text-file "banks-watchdog-start"
    "#!/bin/sh\nset -e\n"
    "FRAG=\"" mm-fragment "\"\n"
    "[ -f \"$FRAG\" ] || { echo 'banks-watchdog: waiting for MM fragment' >&2; sleep 10; exit 1; }\n"
    "set -a\n. \"$FRAG\"\nset +a\n"
    "export BANKS_LEDGER=\"" ledger-root "/main.beancount\"\n"
    "export BANKS_ALERT_USERNAME=\"" alert-username "\"\n"
    ". " (file-append banks-watchdog-profile "/etc/profile") "\n"
    ;; `herd' (for self-healing) comes from the home profile, not this one.
    "export PATH=\"$HOME/.guix-home/profile/bin:$PATH\"\n"
    "exec " (file-append banks-watchdog-profile "/bin/banks-watchdog") "\n"))

(define (banks-watchdog-shepherd-service mm-fragment ledger-root alert-username)
  (list
   (shepherd-service
    (documentation "Liveness watch for banks-bridge (alerts the finance owner)")
    (provision '(banks-watchdog))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh")
                    #$(banks-watchdog-start-script mm-fragment ledger-root
                                                   alert-username))
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/banks-watchdog.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-delay 15)
    (respawn-limit #~'(10 . 60)))))

;;; --- monthly statement -----------------------------------------------------
;;;
;;; Banks otherwise speaks only when spoken to, which makes him useful to
;;; whoever remembers to ask.  Once a month he reports unprompted.  The agent
;;; computes the figures with his own tools inside the sandbox; banks-digest is
;;; scheduler + transport only.

(define banks-digest-profile
  (profile (content (packages->manifest (list banks-digest)))))

(define (banks-digest-run-script mm-fragment)
  (mixed-text-file "banks-digest-run"
    "#!/bin/sh\nset -e\n"
    "FRAG=\"" mm-fragment "\"\n"
    "[ -f \"$FRAG\" ] || { echo 'banks-digest: no MM fragment; skipping' >&2; exit 0; }\n"
    "set -a\n. \"$FRAG\"\nset +a\n"
    ". " (file-append banks-digest-profile "/etc/profile") "\n"
    ;; The `banks' wrapper (which injects the key + ledger paths) is in the
    ;; home profile; the digest shells out to it.
    "export PATH=\"$HOME/.guix-home/profile/bin:$PATH\"\n"
    "exec " (file-append banks-digest-profile "/bin/banks-digest") " \"$@\"\n"))

(define (banks-digest-mcron-job mm-fragment schedule)
  (list
   #~(job #$schedule
          (lambda ()
            (let ((log (string-append (getenv "HOME")
                                      "/.local/state/banks-digest.log")))
              ;; Through sh explicitly: `mixed-text-file' lands in the store
              ;; read-only (0444), so executing the path directly is a
              ;; Permission denied — which a monthly job would only reveal a
              ;; month later.
              (system (string-append #$(file-append bash-minimal "/bin/sh") " "
                                     #$(banks-digest-run-script mm-fragment)
                                     " >> " log " 2>&1"))))
          "banks-digest")))

(define* (banks-home-service
          #:key (openrouter-env-file "/run/secrets/hermes-household/env")
                (ledger-root %default-ledger-root)
                (household-file %default-household-file)
                (mm-fragment %mm-fragment)
                (mm-origin "https://mattermost.drake-karat.ts.net")
                (ops-root %default-ops-root)
                (docs-root %default-docs-root)
                (alert-username "rafael")
                ;; 1st of the month, 09:00 — the month just ended is complete
                ;; and the household is awake to read it.
                (digest-schedule "0 9 1 * *"))
  "Deploy `banks' (pi + the Mr. Banks wrapper) into the home profile, plus the
Mattermost bridge daemon that exposes him on the family finance channel, a
liveness watchdog that alerts ALERT-USERNAME when he goes quiet, and the monthly
statement job."
  (list
   (simple-service 'banks
                   home-profile-service-type
                   (list pi (banks-cli openrouter-env-file ledger-root household-file
                                       docs-root)))
   (simple-service 'banks-bridge
                   home-shepherd-service-type
                   (banks-bridge-shepherd-service mm-fragment mm-origin
                                                  ops-root ledger-root docs-root))
   (simple-service 'banks-watchdog
                   home-shepherd-service-type
                   (banks-watchdog-shepherd-service mm-fragment ledger-root
                                                    alert-username))
   (simple-service 'banks-digest
                   home-mcron-service-type
                   (banks-digest-mcron-job mm-fragment digest-schedule))))
