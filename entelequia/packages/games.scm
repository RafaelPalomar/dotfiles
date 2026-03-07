(define-module (entelequia packages games)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages fontutils)  ; freetype
  #:use-module (gnu packages linux)      ; util-linux, eudev, libcap
  #:use-module (gnu packages xorg)       ; libx11, libxrandr, libxfixes, etc.
  #:use-module (gnu packages gl)         ; mesa
  #:use-module (gnu packages audio)      ; openal
  #:use-module (gnu packages xiph)       ; libogg
  #:use-module (gnu packages gcc)        ; gcc "lib"
  #:use-module (gnu packages sdl)        ; sdl2
  #:use-module (gnu packages gtk)        ; gtk+ (GTK3)
  #:use-module (gnu packages glib)       ; glib
  #:export (make-gog-launcher
            make-gog-fhs-launcher
            gog-crypt-of-the-necrodancer
            gog-terraria))

;;;
;;; GOG game launcher helpers
;;;
;;; Three-tier architecture for running GOG games on Guix:
;;;
;;;   Tier 1/2 — make-gog-launcher
;;;     Embeds Guix store lib paths in LD_LIBRARY_PATH at build time.
;;;     Paths are refreshed automatically on 'guix home reconfigure'.
;;;     Tier 2 works the same way but accepts pinned/custom package inputs.
;;;
;;;   Tier 3 — make-gog-fhs-launcher
;;;     Wraps the game in 'guix shell --container --emulate-fhs'.
;;;     Slower startup (profile built on first run, cached after) but
;;;     handles complex library probing and unknown runtime deps.
;;;

;;; Tier 1/2 — LD_LIBRARY_PATH wrapper

(define* (make-gog-launcher launcher-name game-subdir binary lib-inputs
                             #:key (extra-env '())
                                   (desktop-name launcher-name)
                                   (desktop-icon "applications-games"))
  "Return a package that installs a shell wrapper under bin/LAUNCHER-NAME.

The wrapper sets LD_LIBRARY_PATH to:
  $GAMEDIR/lib (bundled game libs, highest priority)
  + /gnu/store paths for each package in LIB-INPUTS that has a /lib dir

GAME-SUBDIR is relative to $HOME (e.g. \"GOG Games/Foo/game\").
BINARY is the executable name inside GAME-SUBDIR.
EXTRA-ENV is an alist of (\"VAR\" . \"VALUE\") environment variables.

Refresh store paths after 'guix pull' with: guix home reconfigure"
  (package
    (name launcher-name)
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils) (ice-9 format) (srfi srfi-1))
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (launcher (string-append bin "/" ,launcher-name))
                (lib-dirs (filter-map
                           (lambda (entry)
                             (let ((lib (string-append (cdr entry) "/lib")))
                               (and (file-exists? lib) lib)))
                           %build-inputs)))
           (mkdir-p bin)
           (call-with-output-file launcher
             (lambda (port)
               (format port "#!/bin/sh~%")
               (format port "# GOG game launcher: ~a~%" ,launcher-name)
               (format port "# Store paths embedded at build time.~%")
               (format port "# Run 'guix home reconfigure' after 'guix pull' to refresh.~%")
               (format port "GAMEDIR=\"${HOME}/~a\"~%" ,game-subdir)
               (format port "export LD_LIBRARY_PATH=\"${GAMEDIR}/lib:${GAMEDIR}/lib64")
               (for-each (lambda (p) (format port ":~a" p)) lib-dirs)
               (format port "\"~%")
               ,@(map (lambda (pair)
                        `(format port "export ~a=\"~a\"~%"
                                 ,(car pair) ,(cdr pair)))
                      extra-env)
               (format port "cd \"${GAMEDIR}\"~%")
               (format port "exec \"${GAMEDIR}/~a\" \"$@\"~%" ,binary)))
           (chmod launcher #o755)
           ;; .desktop file — Exec calls the launcher by name (on PATH after
           ;; guix home reconfigure).  Icon uses ~ which most DEs expand.
           (let* ((apps    (string-append out "/share/applications"))
                  (desktop (string-append apps "/" ,launcher-name ".desktop")))
             (mkdir-p apps)
             (call-with-output-file desktop
               (lambda (port)
                 (format port "[Desktop Entry]~%")
                 (format port "Version=1.0~%")
                 (format port "Type=Application~%")
                 (format port "Name=~a~%" ,desktop-name)
                 (format port "Comment=GOG game~%")
                 (format port "Exec=~a~%" ,launcher-name)
                 (format port "Icon=~a~%" ,desktop-icon)
                 (format port "Categories=Game;~%")
                 (format port "Terminal=false~%"))))))))
    (inputs lib-inputs)
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "GOG game launcher for " launcher-name))
    (description
     (string-append
      "Shell wrapper for the GOG game " launcher-name ".  "
      "Sets LD_LIBRARY_PATH to Guix store paths at build time.  "
      "Run 'guix home reconfigure' after 'guix pull' to refresh paths."))
    (home-page "https://www.gog.com")
    (license license:expat)))

;;; Tier 3 — FHS container wrapper

(define* (make-gog-fhs-launcher launcher-name game-subdir binary shell-pkgs
                                 #:key (gpu 'amd) (extra-expose '()))
  "Return a package that installs a guix-shell FHS container launcher.

SHELL-PKGS is a list of package-name strings to pass to 'guix shell'.
GPU is 'amd or 'nvidia (adds /dev/nvidia* expose on nvidia).
EXTRA-EXPOSE is a list of device/path strings for --expose."
  (package
    (name launcher-name)
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:builder
       (begin
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (launcher (string-append bin "/" ,launcher-name)))
           (mkdir-p bin)
           (call-with-output-file launcher
             (lambda (port)
               (display "#!/bin/sh\n" port)
               (display "# GOG FHS container launcher\n" port)
               (display "# Runs the game inside 'guix shell --container --emulate-fhs'.\n" port)
               (format port "GAMEDIR=\"${HOME}/~a\"\n" ,game-subdir)
               (display "exec guix shell --container --emulate-fhs \\\n" port)
               (display "  --preserve='^DISPLAY$' \\\n" port)
               (display "  --preserve='^XAUTHORITY$' \\\n" port)
               (display "  --preserve='^PULSE' \\\n" port)
               (display "  --preserve='^XDG_' \\\n" port)
               (display "  --share=\"${HOME}\" \\\n" port)
               (display "  --share=/tmp/.X11-unix \\\n" port)
               (display "  --expose=/dev/dri \\\n" port)
               (display "  --expose=/dev/input \\\n" port)
               ,@(if (eq? gpu 'nvidia)
                    '((display "  --expose=/dev/nvidia0 \\\n" port)
                      (display "  --expose=/dev/nvidiactl \\\n" port)
                      (display "  --expose=/dev/nvidia-modeset \\\n" port))
                    '())
               ,@(map (lambda (e) `(format port "  --expose=~a \\\n" ,e))
                      extra-expose)
               ,@(map (lambda (pkg)
                        `(format port "  ~a \\\n" ,pkg))
                      shell-pkgs)
               (format port
                "  -- env LD_LIBRARY_PATH=\"${GAMEDIR}/lib\" \\\n")
               (format port
                "     sh -c 'cd \"$1\" && exec \"$2\" \"$@\"' -- \\\n")
               (format port
                "     \"${GAMEDIR}\" \"${GAMEDIR}/~a\"\n" ,binary)))
           (chmod launcher #o755)))))
    (inputs '())
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "GOG FHS container launcher for " launcher-name))
    (description
     (string-append
      "FHS container wrapper for the GOG game " launcher-name ".  "
      "Runs inside 'guix shell --container --emulate-fhs' for maximum "
      "compatibility with games that probe /lib64, /usr, etc."))
    (home-page "https://www.gog.com")
    (license license:expat)))

;;;
;;; Per-game package definitions
;;;

;;; Crypt of the NecroDancer — Tier 1
;;;
;;; ldd output confirmed these are the missing libs for the x64 binary.
;;; All are current Guix versions; no pinning required.

(define-public gog-crypt-of-the-necrodancer
  (make-gog-launcher
   "necrodancer"
   "GOG Games/Crypt of the NecroDancer/game/NecroDancer64"
   "NecroDancer.x64"
   (list freetype
         `(,util-linux "lib")
         eudev
         libcap
         libxrandr
         libxfixes
         libxcursor
         libx11
         libxi
         libxinerama
         libxxf86vm
         libxscrnsaver
         libxext
         mesa
         openal
         libogg
         `(,gcc "lib"))
   #:desktop-name "Crypt of the NecroDancer"
   #:desktop-icon "~/GOG Games/Crypt of the NecroDancer/support/icon.png"))

;;; Terraria — Tier 1
;;;
;;; MonoKickstart binary (Terraria.bin.x86_64) with bundled Mono runtime.
;;; Bundled libs: libFAudio, libFNA3D, libSDL3, libnfd variants.
;;; libnfd.so needs libSDL2, libnfd_gtk.so needs GTK3 + glib.
;;; MONO_IOMAP=all required for case-insensitive asset paths (Windows→Linux).

(define-public gog-terraria
  (make-gog-launcher
   "terraria"
   "GOG Games/Terraria/game"
   "Terraria.bin.x86_64"
   (list sdl2
         gtk+
         glib
         pipewire
         `(,gcc "lib"))
   #:extra-env '(("MONO_IOMAP" . "all"))
   #:desktop-name "Terraria"
   #:desktop-icon "~/GOG Games/Terraria/support/icon.png"))
