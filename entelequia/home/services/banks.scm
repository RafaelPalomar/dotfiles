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
  #:use-module (guix-agentic agents core)                  ; agent->package, agent->manifest-entries
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:use-module (gnu home services shepherd)                ; home-shepherd-service-type
  #:use-module (gnu services shepherd)                     ; shepherd-service, forkexec
  #:use-module (gnu packages bash)                         ; bash-minimal (/bin/sh)
  #:export (banks-home-service))

;;; banks — Mr. Banks, the household finance agent (PERSONAL domain, DIRECT).
;;;
;;; Belongs on the family server (edison).  He is Anthropic trusted-tier and talks
;;; to the family through his OWN `ms-banks' bot — Poppins never relays his figures
;;; (arch-review B1).  The wrapper injects, at launch, the secrets the L1 container
;;; scrubs:
;;;   ANTHROPIC_API_KEY <- ANTHROPIC-KEY-FILE   (sops-decrypted, owner = running user)
;;; and sets the non-secret MR_BANKS_LEDGER (absolute path to the read-only ledger
;;; the sandbox --expose's).  agent.scm's sandbox preserves both.
;;;
;;; Deploy prereqs (system side): sops-secret for ANTHROPIC-KEY-FILE; the beancount
;;; ledger present at LEDGER-ROOT, owner-segregated + read-only (arch-review B5);
;;; the `ms-banks' bot provisioned with its token/channel in MM-FRAGMENT.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

(define %default-ledger-root "/var/lib/mr-banks/ledger")
(define %default-household-file "/var/lib/mr-banks/household.md")

(define (banks-wrapper anthropic-key-file ledger-root household-file)
  "An executable `banks' that injects ANTHROPIC_API_KEY + MR_BANKS_LEDGER +
MR_BANKS_HOUSEHOLD and execs the banks launcher with its tool profile on
GUIX_ENVIRONMENT (so the `mr-banks' CLI + the skill land on PATH inside the L1
container)."
  (let* ((banks-agent (make-banks #:ledger-root ledger-root
                                  #:household-file household-file))
         (banks-launcher (agent->package banks-agent))
         (tool-profile (profile (content (packages->manifest
                                          (agent->manifest-entries banks-agent))))))
    (program-file
     "banks"
     #~(begin
         (use-modules (ice-9 rdelim))
         (let ((kf #$anthropic-key-file))
           (when (file-exists? kf)
             (call-with-input-file kf
               (lambda (p) (let ((k (read-line p)))
                             (when (string? k) (setenv "ANTHROPIC_API_KEY" k)))))))
         ;; Absolute path to the read-only ledger root the sandbox --expose's;
         ;; mrbanks.py resolves MR_BANKS_LEDGER via os.path.abspath, so this is safe.
         (setenv "MR_BANKS_LEDGER" (string-append #$ledger-root "/main.beancount"))
         ;; Household roster (names/ages) — private, --expose'd read-only; the
         ;; persona reads member names from this path (never baked into the channel).
         (setenv "MR_BANKS_HOUSEHOLD" #$household-file)
         (unless (getenv "GUIX_ENVIRONMENT")
           (setenv "GUIX_ENVIRONMENT" #$tool-profile))
         ;; The launcher symlinks skills into the config dir from this search path.
         (setenv "GUIX_AGENTIC_PI_SKILL_PATH"
                 (string-append #$tool-profile "/share/pi/skills"))
         (apply execl #$(file-append banks-launcher "/bin/banks")
                "banks" (cdr (command-line)))))))

(define (banks-cli anthropic-key-file ledger-root household-file)
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
               (copy-file #$(banks-wrapper anthropic-key-file ledger-root household-file)
                          (string-append #$output "/bin/banks"))
               (chmod (string-append #$output "/bin/banks") #o755))))
    (synopsis "Mr. Banks launcher wrapper (injects ANTHROPIC_API_KEY + MR_BANKS_LEDGER)")
    (description "Wrapper that injects the Anthropic key (from a sops-decrypted
file) and the ledger path, then execs the banks launcher.")
    (home-page "https://github.com/RafaelPalomar/mr-banks")
    (license license:gpl3+)))

;;; --- the Mattermost bridge (chat surface) ----------------------------------

(define %mm-fragment "/var/lib/mattermost-provision/ms-banks.env")

(define banks-bridge-profile
  (profile (content (packages->manifest (list banks-bridge)))))

(define (banks-bridge-start-script mm-fragment mm-origin)
  (mixed-text-file "banks-bridge-start"
    "#!/bin/sh\nset -e\n"
    "FRAG=\"" mm-fragment "\"\n"
    "[ -f \"$FRAG\" ] || { echo 'banks-bridge: waiting for MM fragment' >&2; sleep 10; exit 1; }\n"
    "set -a\n. \"$FRAG\"\nset +a\n"
    "export MATTERMOST_ORIGIN=\"" mm-origin "\"\n"
    ". " (file-append banks-bridge-profile "/etc/profile") "\n"
    "export PATH=\"$HOME/.guix-home/profile/bin:$PATH\"\n"
    "exec " (file-append banks-bridge-profile "/bin/banks-bridge") "\n"))

(define (banks-bridge-shepherd-service mm-fragment mm-origin)
  (list
   (shepherd-service
    (documentation "Mr. Banks Mattermost bridge (ms-banks bot -> banks -p)")
    (provision '(banks-bridge))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh")
                    #$(banks-bridge-start-script mm-fragment mm-origin))
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/banks-bridge.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define* (banks-home-service
          #:key (anthropic-key-file "/run/secrets/anthropic/banks")
                (ledger-root %default-ledger-root)
                (household-file %default-household-file)
                (mm-fragment %mm-fragment)
                (mm-origin "https://mattermost.drake-karat.ts.net"))
  "Deploy `banks' (pi + the Mr. Banks wrapper) into the home profile, plus the
Mattermost bridge daemon that exposes him on the family finance channel."
  (list
   (simple-service 'banks
                   home-profile-service-type
                   (list pi (banks-cli anthropic-key-file ledger-root household-file)))
   (simple-service 'banks-bridge
                   home-shepherd-service-type
                   (banks-bridge-shepherd-service mm-fragment mm-origin))))
