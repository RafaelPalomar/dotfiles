(define-module (entelequia home services pks)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-pks-service-type))

;;; PKS home service
;;;
;;; Personal Knowledge System integration:
;;;   * Bootstraps ~/Nextcloud/PKS/ silo tree on every `guix home reconfigure`.
;;;   * Maintains the ~/pks symlink pointing at the Nextcloud-backed tree.
;;;   * Schedules a weekly Nextcloud-conflict scan via mcron.
;;;
;;; The silo tree is the denote-compatible knowledge base described in
;;; the repo plan (function-based Zettelkasten: fleeting/permanent/
;;; literature/projects/reference/review-queue + library/ for raw
;;; material).  See /home/rafael/.claude/plans/we-are-going-to-cheerful-clarke.md.

(define pks-bootstrap-activation
  #~(begin
      (use-modules (ice-9 ftw)
                   (ice-9 match))
      (let* ((home      (getenv "HOME"))
             (nc-root   (string-append home "/Nextcloud/PKS"))
             (pks-link  (string-append home "/pks"))
             (subdirs   '("fleeting" "permanent" "literature"
                          "projects" "reference" "review-queue"
                          "library"
                          "library/papers" "library/books"
                          "library/data"   "library/scans")))
        ;; 1. Ensure the Nextcloud-backed PKS tree exists.  We can only
        ;;    create our own tree; if ~/Nextcloud itself is absent we
        ;;    bail quietly — the user hasn't started the Nextcloud
        ;;    client yet, and pks-migrate.sh will handle first-time
        ;;    bootstrap explicitly.
        (when (file-exists? (string-append home "/Nextcloud"))
          (for-each (lambda (d)
                      (let ((p (string-append nc-root "/" d)))
                        (unless (file-exists? p)
                          (mkdir-p p))))
                    subdirs)
          ;; 2. Ensure ~/pks symlinks into ~/Nextcloud/PKS.
          ;;    Do not clobber an existing symlink or directory —
          ;;    migration script is the only tool that converts a real
          ;;    directory into a symlink.
          (unless (file-exists? pks-link)
            (symlink nc-root pks-link))))))

(define (home-pks-conflict-scan-job config)
  "Weekly Nextcloud-conflict scanner.  Runs pks-conflicts and logs to
~/.local/state/pks-conflicts.log.  No-op if the script is missing."
  (list
   #~(job "0 9 * * 1"                    ; Mondays 09:00 local
          (lambda ()
            (let* ((home   (getenv "HOME"))
                   (script (string-append home "/.local/bin/pks-conflicts"))
                   (log    (string-append home "/.local/state/pks-conflicts.log")))
              (when (file-exists? script)
                (mkdir-p (dirname log))
                (system (string-append script " >> " log " 2>&1")))))
          "pks-conflicts")))

(define home-pks-service-type
  (service-type
   (name 'home-pks)
   (description
    "Bootstrap ~/Nextcloud/PKS/ silo tree + ~/pks symlink; schedule a
weekly Nextcloud-conflict scan.")
   (extensions
    (list (service-extension
           home-activation-service-type
           (const pks-bootstrap-activation))
          (service-extension
           home-mcron-service-type
           home-pks-conflict-scan-job)))
   (default-value #f)))
