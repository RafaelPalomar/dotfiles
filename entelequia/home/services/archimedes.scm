(define-module (entelequia home services archimedes)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix profiles)                             ; profile
  #:use-module (archimedes agent)                          ; archimedes-launcher (pinned channel)
  #:use-module (archimedes manifests archimedes)           ; archimedes-manifest (launcher + tools)
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:export (archimedes-home-service))

;;; Per-child Archimedes deployment (home service).
;;;
;;; Installs, into the child's home profile:
;;;   - `pi'  (the coding-agent CLI the launcher execs), and
;;;   - `archimedes' — a thin wrapper that injects the secret/identity the
;;;     neutral channel expects and execs the real launcher:
;;;       * OPENROUTER_API_KEY  <- contents of KEY-FILE  (default
;;;         /run/secrets/openrouter/<learner>), where *sops-guix* has decrypted
;;;         the key at boot with the right owner/permissions — NOT from `pass'
;;;         (the key lives in the user's pass on curie, not on the target).
;;;       * ARCHIMEDES_LEARNER  = <learner>
;;;     The child's NAME is NOT injected — the owl learns it through dialogue
;;;     and records it in the Denote journal (no PII in config).
;;;
;;; System prerequisite: alucard's system must run sops-secrets-service-type with
;;;   (sops-secret (key '("openrouter" "<learner>")) (file %sops-alucard) ...)
;;; so KEY-FILE exists and is readable by the child.  See system/machines/alucard.scm.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

;; The agent's tool closure as a store profile: everything in the channel's
;; launch manifest (launcher + ripgrep + fd + python/pgzero + denotecli + the
;; memory tools).  Pointing GUIX_ENVIRONMENT at this profile makes the launcher
;; reuse it (`guix shell -C -p <profile>'), which is the ONLY way the tools land
;; on PATH *inside* the L1 container — a bare launch otherwise --exposes them
;; without ever adding their bin/ to PATH, so pi falls back to downloading
;; rg/fd.  Learner-independent, so one profile serves every child.  `pi' itself
;; is resolved from the home profile at the outer level, per the channel design.
(define archimedes-tool-profile
  (profile (content archimedes-manifest)))

(define (archimedes-wrapper learner key-file)
  "An executable `archimedes' that sets ARCHIMEDES_LEARNER, reads the OpenRouter
key from KEY-FILE (sops-decrypted local file), and execs the launcher."
  (program-file
   "archimedes"
   #~(begin
       (use-modules (ice-9 rdelim))
       (let* ((home (or (getenv "HOME") "/tmp"))
              (journal (string-append home "/.local/share/archimedes/learners"))
              (key-file #$key-file))
         (system* "mkdir" "-p" journal)
         (setenv "ARCHIMEDES_LEARNER" #$learner)
         ;; sops-guix decrypts the key to KEY-FILE at boot (owner = this user,
         ;; mode 0400).  Read it at launch; it never lands anywhere else.
         (when (file-exists? key-file)
           (call-with-input-file key-file
             (lambda (p)
               (let ((k (read-line p)))
                 (when (string? k) (setenv "OPENROUTER_API_KEY" k))))))
         ;; Hand the launcher a ready-built tool profile to reuse inside the
         ;; container, so rg/fd/denotecli are on PATH and nothing is downloaded.
         (unless (getenv "GUIX_ENVIRONMENT")
           (setenv "GUIX_ENVIRONMENT" #$archimedes-tool-profile))
         (apply execl #$(file-append archimedes-launcher "/bin/archimedes")
                "archimedes" (cdr (command-line)))))))

(define (archimedes-cli learner key-file)
  "A package placing the wrapper at bin/archimedes (so it lands on PATH via the
home profile)."
  (package
    (name (string-append "archimedes-" learner))
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(archimedes-wrapper learner key-file)
                          (string-append #$output "/bin/archimedes"))
               (chmod (string-append #$output "/bin/archimedes") #o755))))
    (synopsis (string-append "Archimedes launcher for " learner))
    (description "Per-child wrapper that injects the OpenRouter key (from the
sops-decrypted local file) and the learner id, then execs the Archimedes
launcher.")
    (home-page "https://github.com/RafaelPalomar/archimedes-agent")
    (license license:gpl3+)))

(define* (archimedes-home-service #:key learner
                                  (key-file (string-append
                                             "/run/secrets/openrouter/" learner)))
  "Return the home services that deploy Archimedes for LEARNER: `pi' and the
`archimedes' wrapper (reading the OpenRouter key from KEY-FILE), both into the
home profile."
  (unless (string? learner) (error "archimedes-home-service: #:learner required"))
  (list
   (simple-service (string->symbol (string-append "archimedes-" learner))
                   home-profile-service-type
                   (list pi (archimedes-cli learner key-file)))))
