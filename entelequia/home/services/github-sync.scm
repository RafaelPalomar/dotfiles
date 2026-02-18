(define-module (entelequia home services github-sync)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:export (home-github-sync-service-type))

;;; GitHub Sync Home Service
;;;
;;; Provides automatic synchronization of GitHub issues and PRs to org-mode
;;; every 60 minutes. Runs as a Guix Shepherd service with auto-restart on
;;; failure and logging to ~/.local/state/org-github-sync.log.

(define (home-github-sync-shepherd-service config)
  "Return a Shepherd service for GitHub to org-mode synchronization."
  (list
   (shepherd-service
    (documentation "Periodic GitHub to org-mode sync (every 60 minutes)")
    (provision '(github-sync))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/bash")
                    "-c"
                    (string-append
                     "while true; do "
                     (getenv "HOME") "/.local/bin/org-github-sync-enhanced.sh; "
                     "sleep 3600; "  ; 60 minutes
                     "done"))
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/org-github-sync.log")
              #:environment-variables
              (cons* (string-append "PATH=" (getenv "PATH"))
                     (string-append "HOME=" (getenv "HOME"))
                     (default-environment-variables))))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (auto-start? #t))))

(define home-github-sync-service-type
  (service-type
   (name 'home-github-sync)
   (description "Automatic GitHub to org-mode synchronization service")
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-github-sync-shepherd-service)))
   (default-value #f)))
