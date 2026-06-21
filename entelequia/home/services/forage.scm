(define-module (entelequia home services forage)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix profiles)                             ; profile, concatenate-manifests
  #:use-module (gnu packages rust-apps)                    ; fd
  #:use-module (alpha-agent forager)                       ; forager-launcher (pinned channel)
  #:use-module (alpha-agent manifests forager)             ; forager-tool-profile
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:use-module (gnu home services shepherd)                ; home-shepherd-service-type
  #:use-module (gnu services shepherd)                     ; shepherd-service, forkexec
  #:use-module (gnu packages bash)                         ; bash-minimal (/bin/sh for the broker)
  #:export (forage-home-service))

;;; forage — the queen's forager-dispatch entrypoint (Stage 1 of the colony).
;;;
;;; `forage' is how a QUEEN running on the host (e.g. Claude Code, or a human)
;;; spawns a one-shot forager and reads back its <report>.  It is NOT a daemon:
;;; each invocation launches a fresh, isolated forager (Haiku, no PKS, no SSH,
;;; no episodic store) in its own L1 container, exactly like `alpha' but with
;;; the governance capabilities stripped (see alpha-agent/forager.scm).
;;;
;;; Usage (queen side):
;;;     echo "<task>" | forage -p        # dispatch, read the <report> on stdout
;;;
;;; The wrapper does the two things a bare launcher can't:
;;;   1. inject OPENROUTER_API_KEY from the sops-decrypted KEY-FILE, and
;;;   2. point GUIX_ENVIRONMENT at the forager tool profile so codegraph / git /
;;;      ripgrep / findutils land on PATH *inside* the container (otherwise pi
;;;      would try to download them).
;;; It deliberately does NOT set DENOTECLI_DIRS — the forager has no PKS.
;;;
;;; alpha-as-queen does NOT use this (it is sandboxed without the daemon); that
;;; path needs the host-side dispatch broker (Stage 2).

(define pi node-earendil-works-pi-coding-agent-0.78.1)

;; The forager's full tool closure as a store profile (launcher + codegraph +
;; git/ripgrep/findutils + the skills), reused inside the L1 container via
;; `guix shell -C -p <profile>'.  Same trick as alpha-tool-profile; fd added for
;; parity with alpha's toolset.
(define forage-tool-profile
  (profile
   (content (concatenate-manifests
             (list forager-manifest
                   (packages->manifest (list fd)))))))

(define (forage-wrapper key-file)
  "An executable `forage' that reads the OpenRouter key from KEY-FILE and execs
the forager launcher with the forager tool profile on GUIX_ENVIRONMENT."
  (program-file
   "forage"
   #~(begin
       (use-modules (ice-9 rdelim))
       (let ((key-file #$key-file))
         (when (file-exists? key-file)
           (call-with-input-file key-file
             (lambda (p)
               (let ((k (read-line p)))
                 (when (string? k) (setenv "OPENROUTER_API_KEY" k))))))
         ;; NB: no DENOTECLI_DIRS — the forager is cut off from the PKS.
         (unless (getenv "GUIX_ENVIRONMENT")
           (setenv "GUIX_ENVIRONMENT" #$forage-tool-profile))
         (apply execl #$(file-append forager-launcher "/bin/forager")
                "forager" (cdr (command-line)))))))

(define (forage-cli key-file)
  "A package placing the wrapper at bin/forage."
  (package
    (name "forage-cli")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(forage-wrapper key-file)
                          (string-append #$output "/bin/forage"))
               (chmod (string-append #$output "/bin/forage") #o755))))
    (synopsis "forager-dispatch entrypoint (queen side of the colony)")
    (description "Wrapper that injects the OpenRouter key and execs a one-shot,
isolated forager launcher, pointing GUIX_ENVIRONMENT at the forager tool profile.")
    (home-page "https://github.com/RafaelPalomar/alpha-agent")
    (license license:gpl3+)))

;;; --- the broker (host side, Stage 2): watch the dispatch dir, spawn ONE
;;; isolated forager per request, write the <report> back.  This is the
;;; PRIVILEGED half — it owns the spawn and (through `forage') the key.  A
;;; request file carries a task, never a command; its `.forager' extension
;;; selects the fixed, isolated agent.  Lets a sandboxed queen (alpha, via the
;;; `with-foraging' dispatch client) delegate without ever touching the daemon.

(define %default-dispatch-dir "/home/rafael/.local/share/agent-dispatch")

(define (forage-broker-script key-file dispatch-dir)
  (mixed-text-file "forage-broker"
    "#!/bin/sh\nset -u\n"
    "DIR=\"" dispatch-dir "\"\n"
    "FORAGE=\"" (file-append (forage-cli key-file) "/bin/forage") "\"\n"
    "mkdir -p \"$DIR/requests\" \"$DIR/reports\" \"$DIR/done\"\n"
    "while :; do\n"
    "  for req in \"$DIR\"/requests/*.forager; do\n"
    "    [ -e \"$req\" ] || continue\n"
    "    id=\"$(basename \"$req\" .forager)\"; rep=\"$DIR/reports/$id.report\"\n"
    "    if [ ! -f \"$rep\" ]; then\n"
    "      \"$FORAGE\" -p < \"$req\" > \"$rep.part\" 2>\"$DIR/reports/$id.err\" || true\n"
    "      mv \"$rep.part\" \"$rep\" 2>/dev/null || true\n"
    "    fi\n"
    "    mv \"$req\" \"$DIR/done/$id.forager\" 2>/dev/null || true\n"
    "  done\n"
    "  sleep 3\n"
    "done\n"))

(define (forage-broker-shepherd-service key-file dispatch-dir)
  (list
   (shepherd-service
    (documentation "Forager broker: spawn an isolated forager per dispatch request")
    (provision '(forage-broker))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh")
                    #$(forage-broker-script key-file dispatch-dir))
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    ;; pi + tools resolve from the home profile; system profile
                    ;; keeps coreutils/sh available to the broker loop.
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/forage-broker.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define (forage-dispatch-dir-activation dispatch-dir)
  ;; The dispatch dir must exist before alpha launches (its sandbox --shares it).
  (simple-service 'forage-dispatch-dir
                  home-activation-service-type
                  (with-imported-modules '((guix build utils))
                    #~(begin
                        (use-modules (guix build utils))
                        (for-each mkdir-p
                                  (list (string-append #$dispatch-dir "/requests")
                                        (string-append #$dispatch-dir "/reports")
                                        (string-append #$dispatch-dir "/done")))))))

(define* (forage-home-service #:key (key-file "/run/secrets/openrouter/rafael")
                              (dispatch-dir %default-dispatch-dir))
  "Deploy `forage' (the queen entrypoint + pi), the forager broker (spawns an
isolated forager per dispatch request), and the shared dispatch dir."
  (list
   (simple-service 'forage
                   home-profile-service-type
                   (list pi (forage-cli key-file)))
   (forage-dispatch-dir-activation dispatch-dir)
   (simple-service 'forage-broker
                   home-shepherd-service-type
                   (forage-broker-shepherd-service key-file dispatch-dir))))
