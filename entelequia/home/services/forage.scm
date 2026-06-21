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

(define* (forage-home-service #:key (key-file "/run/secrets/openrouter/rafael"))
  "Return the home services deploying `forage': `pi' and the `forage' wrapper."
  (list
   (simple-service 'forage
                   home-profile-service-type
                   (list pi (forage-cli key-file)))))
