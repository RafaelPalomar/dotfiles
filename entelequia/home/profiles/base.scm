(define-module (entelequia home profiles base)
  #:use-module (gnu packages)
  #:export (base-home-packages))

;;; Base home profile
;;;
;;; Minimal set of packages for any home environment.
;;; These are the essentials needed for a functional user environment.

(define* (base-home-packages #:key (gpu-type 'nvidia))
  (append
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
          ))
   ;; OpenGL dispatcher — NVIDIA hosts only.
   ;;
   ;; libglvnd's libGL.so.1 dispatches at runtime to a GLX vendor
   ;; library (libGLX_nvidia.so.0 or libGLX_mesa.so.0) based on the
   ;; X server's GLX vendor advertisement.  On NVIDIA hosts this is
   ;; required so that browsers/games/GTK apps don't fall back to
   ;; llvmpipe software rendering when their bundled libGL.so wins
   ;; the profile-union conflict.
   ;;
   ;; On AMD/Intel hosts running Mesa 26.0.2 from upstream Guix,
   ;; Mesa is NOT built with -Dglvnd=true and therefore ships no
   ;; libGLX_mesa.so.0 vendor library.  Installing libglvnd on these
   ;; hosts hijacks libGL.so.1 with a dispatcher that has nothing to
   ;; dispatch to: GLX returns no FBConfigs and clients like kitty,
   ;; glxinfo, and games fail to obtain a GL context.  Diagnosed on
   ;; curie 2026-05-12 — kitty refused to start with "GLX: No
   ;; GLXFBConfigs returned".
   (if (eq? gpu-type 'nvidia)
       (list (specification->package "libglvnd"))
       '())))
