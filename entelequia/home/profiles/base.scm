(define-module (entelequia home profiles base)
  #:use-module (gnu packages)
  #:export (base-home-packages))

;;; Base home profile
;;;
;;; Minimal set of packages for any home environment.
;;; These are the essentials needed for a functional user environment.

(define (base-home-packages)
  (map specification->package
       '(;; Shell utilities
         "bash-completion"
         "coreutils"
         "expect"  ;; For automated interaction with CLI programs

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
         "nano"

         ;; Media/preview tools (for dirvish/file managers)
         "ffmpegthumbnailer"  ;; Video thumbnails
         "imagemagick"        ;; Image processing and previews
         "poppler"            ;; PDF previews (includes pdftotext, pdftocairo)

         ;; OpenGL dispatcher.  Without this in the home profile,
         ;; Mesa's libGL.so.1 (pulled in transitively by browsers,
         ;; games, GTK apps, etc.) wins the profile-union conflict
         ;; against the system libglvnd, and on NVIDIA hosts every
         ;; OpenGL client falls through to llvmpipe / software
         ;; rendering — visible as: glxinfo "Vendor: Mesa", apps
         ;; running at single-digit fps.  libglvnd ships its own
         ;; libGL.so.1 which dispatches at runtime to libGLX_nvidia.so
         ;; or libGLX_mesa.so based on the X server's GLX vendor
         ;; advertisement, so it works correctly on Intel/AMD too.
         "libglvnd"
         )))
