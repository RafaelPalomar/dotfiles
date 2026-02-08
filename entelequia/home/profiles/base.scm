(define-module (entelequia home profiles base)
  #:use-module (gnu packages)
  #:export (base-home-packages))

;;; Base home profile
;;;
;;; Minimal set of packages for any home environment.
;;; These are the essentials needed for a functional user environment.

(define base-home-packages
  (map specification->package
       '(;; Shell utilities
         "bash-completion"
         "coreutils"

         ;; Version control
         "git"

         ;; Terminal utilities
         "grep"
         "sed"
         "gawk"
         "findutils"

         ;; Compression
         "gzip"
         "xz"
         "tar"

         ;; Text processing
         "less"
         "nano")))
