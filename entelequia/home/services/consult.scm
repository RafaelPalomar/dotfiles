(define-module (entelequia home services consult)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)                             ; profile, packages->manifest
  #:use-module (srfi srfi-1)                               ; append-map
  #:use-module (archimedes report)                         ; make-archimedes-report(-launcher)
  #:use-module (guix-agentic agents core)                  ; agent->package, agent->manifest-entries
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:use-module (gnu home services shepherd)                ; home-shepherd-service-type
  #:use-module (gnu services shepherd)                     ; shepherd-service, forkexec
  #:use-module (gnu packages bash)                         ; bash-minimal (/bin/sh)
  #:export (consult-home-service))

;;; consult broker — Stage 2 (host side) of the household cooperation topology.
;;;
;;; Poppins (the queen, on edison) delegates to the children's tutor by writing a
;;; question into the shared consult dispatch dir with a `.archimedes-<child>'
;;; extension (via the `consult' client, guix-agentic capabilities consult).  This
;;; broker watches that dir and, for each request, spawns a ONE-SHOT read-only
;;; reporting-Archimedes for that child (its own L1 sandbox, journal --expose'd
;;; read-only) and writes its stdout back as the report.  The broker is the
;;; privileged half: it owns the spawn + the reporting OpenRouter key.  A request
;;; carries a QUESTION, never a command; the child is fixed by the extension, and
;;; ONLY the declared children get a handler here (defense-in-depth for the closed
;;; allow-list the client already enforces).
;;;
;;; UID-wall caveat (arch-review B1): the strict wall is that the synced journal
;;; tree (LEARNERS-ROOT) is owned by a dedicated read-only user and NOT readable by
;;; Poppins's process user.  A home service runs as ONE user, so running this
;;; broker as that dedicated user is the clean form; on a single-user family box the
;;; boundary is the L1 sandbox (Poppins's container never --expose's the journal).
;;; Deploy accordingly; see the Slice-A deploy checklist.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

(define %default-consult-dir "/home/rafael/.local/share/consult-dispatch")
;;; The synced journal tree on edison (NOT the child's ~/.local/... — that lives on
;;; the kid's laptop and syncs here).  Baked into the reporting agent's read-only
;;; --expose at build time (arch-review B2: pass it explicitly, never eval-time HOME).
(define %default-learners-root "/var/lib/archimedes-journals")

;;; --- per-child reporting wrapper (one per child; learner baked, B4) ---------

(define (report-wrapper child key-file report-launcher tool-profile)
  "An executable that sets ARCHIMEDES_LEARNER=CHILD, injects the reporting
OpenRouter key from KEY-FILE, points GUIX_ENVIRONMENT at TOOL-PROFILE (so denotecli
is on PATH inside the container), and execs the reporting launcher.  It does NOT
create or touch the journal — that tree is read-only and synced from the child's
machine."
  (program-file
   (string-append "archimedes-report-" child)
   #~(begin
       (use-modules (ice-9 rdelim))
       (setenv "ARCHIMEDES_LEARNER" #$child)
       (let ((kf #$key-file))
         (when (file-exists? kf)
           (call-with-input-file kf
             (lambda (p)
               (let ((k (read-line p)))
                 (when (string? k) (setenv "OPENROUTER_API_KEY" k)))))))
       (unless (getenv "GUIX_ENVIRONMENT")
         (setenv "GUIX_ENVIRONMENT" #$tool-profile))
       (apply execl #$(file-append report-launcher "/bin/archimedes-report")
              "archimedes-report" (cdr (command-line))))))

;;; --- the broker loop --------------------------------------------------------

(define (consult-broker-script consult-dir child+wrapper)
  "Watch CONSULT-DIR/requests for `*.archimedes-<child>' (only for the children in
CHILD+WRAPPER, an alist of (child . wrapper-file)); spawn that child's reporting
wrapper one-shot, capture stdout as the report."
  (apply mixed-text-file "consult-broker"
    "#!/bin/sh\nset -u\n"
    "DIR=\"" consult-dir "\"\n"
    "mkdir -p \"$DIR/requests\" \"$DIR/reports\" \"$DIR/done\"\n"
    "while :; do\n"
    (append
     (append-map
      (lambda (cw)
        (let ((child (car cw)) (wrapper (cdr cw)))
          (list
           "  for req in \"$DIR\"/requests/*.archimedes-" child "; do\n"
           "    [ -e \"$req\" ] || continue\n"
           "    id=\"$(basename \"$req\" .archimedes-" child ")\"; rep=\"$DIR/reports/$id.report\"\n"
           "    if [ ! -f \"$rep\" ]; then\n"
           "      " wrapper " -p < \"$req\" > \"$rep.part\" 2>\"$DIR/reports/$id.err\" || true\n"
           "      mv \"$rep.part\" \"$rep\" 2>/dev/null || true\n"
           "    fi\n"
           "    mv \"$req\" \"$DIR/done/$id.archimedes-" child "\" 2>/dev/null || true\n"
           "  done\n")))
      child+wrapper)
     (list "  sleep 3\ndone\n"))))

(define (consult-broker-shepherd-service script)
  (list
   (shepherd-service
    (documentation "Consult broker: spawn a read-only reporting-Archimedes per request")
    (provision '(consult-broker))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh") #$script)
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    ;; pi + tools resolve from the home profile; system profile
                    ;; keeps coreutils/sh available to the loop.
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/consult-broker.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define (consult-dispatch-dir-activation consult-dir)
  ;; Must exist before Poppins launches (her sandbox --shares it rw).
  (simple-service 'consult-dispatch-dir
                  home-activation-service-type
                  (with-imported-modules '((guix build utils))
                    #~(begin
                        (use-modules (guix build utils))
                        (for-each mkdir-p
                                  (list (string-append #$consult-dir "/requests")
                                        (string-append #$consult-dir "/reports")
                                        (string-append #$consult-dir "/done")))))))

(define* (consult-home-service
          #:key (learners-root %default-learners-root)
                (key-file "/run/secrets/openrouter/archimedes-report")
                (learners '("leandro" "adrian"))
                (consult-dir %default-consult-dir))
  "Deploy the consult broker + shared dispatch dir for the household topology.
Spawns a read-only reporting-Archimedes per request over the synced journals at
LEARNERS-ROOT, keyed by the request's `.archimedes-<child>' extension for each
child in LEARNERS.  KEY-FILE is the reporting agent's sops-decrypted OpenRouter key."
  (let* ((report-agent (make-archimedes-report #:learners-root learners-root))
         (report-launcher (agent->package report-agent))
         (tool-profile (profile (content (packages->manifest
                                          (agent->manifest-entries report-agent)))))
         (child+wrapper (map (lambda (c)
                               (cons c (report-wrapper c key-file report-launcher tool-profile)))
                             learners))
         (script (consult-broker-script consult-dir child+wrapper)))
    (list
     (simple-service 'consult-broker-pi home-profile-service-type (list pi))
     (consult-dispatch-dir-activation consult-dir)
     (simple-service 'consult-broker
                     home-shepherd-service-type
                     (consult-broker-shepherd-service script)))))
