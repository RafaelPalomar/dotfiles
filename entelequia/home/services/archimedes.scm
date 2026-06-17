(define-module (entelequia home services archimedes)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (archimedes agent)                          ; archimedes-launcher (pinned channel)
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:export (archimedes-home-service))

;;; Per-child Archimedes deployment (home service).
;;;
;;; Installs, into the child's home profile:
;;;   - `pi'  (the coding-agent CLI the launcher execs), and
;;;   - `archimedes' — a thin wrapper that injects the secrets/identity the
;;;     neutral channel expects and execs the real launcher:
;;;       * OPENROUTER_API_KEY  <- `pass show api/openrouter_<learner>'  (runtime)
;;;       * ARCHIMEDES_LEARNER  = <learner>                              (fixed)
;;;     The child's NAME is NOT injected — the owl learns it through
;;;     dialogue and records it in the Denote journal (no PII in config).
;;;
;;; The wrapper references the launcher by store path, so the launcher's own
;;; `guix shell -C' sandbox (and persona/skills/memory) ride in via its closure;
;;; pi is on PATH so the launcher's `command -v pi' resolves.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

(define (archimedes-wrapper learner)
  "An executable `archimedes' that sets the env from `pass'/identity and execs
the launcher for LEARNER."
  (program-file
   "archimedes"
   #~(begin
       (use-modules (ice-9 popen) (ice-9 rdelim))
       (let* ((home (or (getenv "HOME") "/tmp"))
              (journal (string-append home "/.local/share/archimedes/learners")))
         (system* "mkdir" "-p" journal)
         (setenv "ARCHIMEDES_LEARNER" #$learner)
         ;; Decrypt the OpenRouter key from pass at launch (gpg-agent is
         ;; unlocked by pam-gnupg on login).  Never stored on disk.
         (let* ((p   (open-input-pipe
                      (string-append "pass show api/openrouter_" #$learner)))
                (key (read-line p)))
           (close-pipe p)
           (when (string? key) (setenv "OPENROUTER_API_KEY" key)))
         (apply execl #$(file-append archimedes-launcher "/bin/archimedes")
                "archimedes" (cdr (command-line)))))))

(define (archimedes-cli learner)
  "A package placing the wrapper at bin/archimedes (so it lands on PATH via the
home profile)."
  (package
    (name (string-append "archimedes-" learner))
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           #~(begin
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(archimedes-wrapper learner)
                          (string-append #$output "/bin/archimedes"))
               (chmod (string-append #$output "/bin/archimedes") #o755))))
    (synopsis (string-append "Archimedes launcher for " learner))
    (description "Per-child wrapper that injects the OpenRouter key (from pass)
and the learner id, then execs the Archimedes agent launcher.")
    (home-page "https://github.com/RafaelPalomar/archimedes-agent")
    (license license:gpl3+)))

(define* (archimedes-home-service #:key learner)
  "Return the home services that deploy Archimedes for LEARNER: `pi' and the
`archimedes' wrapper, both into the home profile."
  (unless (string? learner) (error "archimedes-home-service: #:learner required"))
  (list
   (simple-service (string->symbol (string-append "archimedes-" learner))
                   home-profile-service-type
                   (list pi (archimedes-cli learner)))))
