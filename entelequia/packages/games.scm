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
  #:use-module (gnu packages gtk)        ; gtk+ (GTK3), gtk+-2 (GTK2)
  #:use-module (gnu packages glib)       ; glib
  #:export (make-game-launcher
            make-game-fhs-launcher
            ;; GOG games
            gog-crypt-of-the-necrodancer
            gog-terraria
            gog-wizard-of-legend
            gog-slay-the-spire
            gog-death-road-to-canada
            ;; Direct-download games
            coq-caves-of-qud
            bay12-dwarf-fortress))

;;;
;;; Game launcher helpers
;;;
;;; Three-tier architecture for running Linux games on Guix:
;;;
;;;   Tier 1/2 — make-game-launcher
;;;     Embeds Guix store lib paths in LD_LIBRARY_PATH at build time.
;;;     Paths are refreshed automatically on 'guix home reconfigure'.
;;;     Also generates a .desktop file so the game appears in app launchers.
;;;
;;;   Tier 3 — make-game-fhs-launcher
;;;     Wraps the game in 'guix shell --container --emulate-fhs'.
;;;     Slower startup (profile built on first run, cached after) but
;;;     handles complex library probing and unknown runtime deps.
;;;

;;; Tier 1/2 — LD_LIBRARY_PATH wrapper

(define* (make-game-launcher launcher-name game-subdir binary lib-inputs
                              #:key (extra-env '())
                                    (extra-lib-dirs '())
                                    (default-args '())
                                    (pre-launch '())
                                    (post-launch '())
                                    (desktop-name launcher-name)
                                    (desktop-icon "applications-games"))
  "Return a package that installs a shell wrapper under bin/LAUNCHER-NAME.

The wrapper sets LD_LIBRARY_PATH to:
  $GAMEDIR/lib (bundled game libs, highest priority)
  $GAMEDIR/lib64
  EXTRA-LIB-DIRS (shell expressions evaluated at run time,
                  e.g. \"${GAMEDIR}/jre/lib/amd64\")
  + /gnu/store paths for each package in LIB-INPUTS that has a /lib dir

Also installs a .desktop file so the game appears in rofi/app menus.

GAME-SUBDIR is relative to $HOME (e.g. \"GOG Games/Foo/game\" or \"Games/Bar\").
BINARY is the executable name inside GAME-SUBDIR.
EXTRA-ENV is an alist of (\"VAR\" . \"VALUE\") environment variables.
EXTRA-LIB-DIRS is a list of shell path strings appended after lib64.
DEFAULT-ARGS is a list of strings prepended before \"$@\" in the exec line
  and baked into the .desktop Exec= field (e.g. '(\"-screen-width\" \"1280\")).
PRE-LAUNCH is a list of shell script lines emitted before the game launch.
POST-LAUNCH is a list of shell script lines emitted after the game exits.
  When POST-LAUNCH is non-empty, 'exec' is replaced by a direct call so that
  post-launch code runs after the game process terminates.

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
               (format port "# Game launcher: ~a~%" ,launcher-name)
               (format port "# Store paths embedded at build time.~%")
               (format port "# Run 'guix home reconfigure' after 'guix pull' to refresh.~%")
               (format port "GAMEDIR=\"${HOME}/~a\"~%" ,game-subdir)
               (format port "export LD_LIBRARY_PATH=\"${GAMEDIR}/lib:${GAMEDIR}/lib64")
               ,@(map (lambda (d) `(format port ":~a" ,d)) extra-lib-dirs)
               (for-each (lambda (p) (format port ":~a" p)) lib-dirs)
               (format port "\"~%")
               ,@(map (lambda (pair)
                        `(format port "export ~a=\"~a\"~%"
                                 ,(car pair) ,(cdr pair)))
                      extra-env)
               (format port "cd \"${GAMEDIR}\"~%")
               ,@(map (lambda (line) `(format port "~a~%" ,line)) pre-launch)
               ,(if (null? post-launch)
                    `(begin
                       (format port "exec \"${GAMEDIR}/~a\"" ,binary)
                       ,@(map (lambda (a) `(format port " ~a" ,a)) default-args)
                       (format port " \"$@\"~%"))
                    `(begin
                       (format port "\"${GAMEDIR}/~a\"" ,binary)
                       ,@(map (lambda (a) `(format port " ~a" ,a)) default-args)
                       (format port " \"$@\"~%")
                       ,@(map (lambda (line) `(format port "~a~%" ,line)) post-launch)))
               ))
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
                 (format port "Exec=~a" ,launcher-name)
                 ,@(map (lambda (a) `(format port " ~a" ,a)) default-args)
                 (format port "~%")
                 (format port "Icon=~a~%" ,desktop-icon)
                 (format port "Categories=Game;~%")
                 (format port "Terminal=false~%"))))))))
    (inputs lib-inputs)
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "Game launcher for " launcher-name))
    (description
     (string-append
      "Shell wrapper for " launcher-name ".  "
      "Sets LD_LIBRARY_PATH to Guix store paths at build time.  "
      "Run 'guix home reconfigure' after 'guix pull' to refresh paths."))
    (home-page "https://www.gnu.org/software/guix/")
    (license license:expat)))

;;; Tier 3 — FHS container wrapper

(define* (make-game-fhs-launcher launcher-name game-subdir binary shell-pkgs
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
               (display "# FHS container game launcher\n" port)
               (display "# Runs inside 'guix shell --container --emulate-fhs'.\n" port)
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
    (synopsis (string-append "FHS container launcher for " launcher-name))
    (description
     (string-append
      "FHS container wrapper for " launcher-name ".  "
      "Runs inside 'guix shell --container --emulate-fhs' for maximum "
      "compatibility with games that probe /lib64, /usr, etc."))
    (home-page "https://www.gnu.org/software/guix/")
    (license license:expat)))

;;;
;;; Per-game package definitions
;;;

;;; ── GOG games ────────────────────────────────────────────────────────────

;;; Crypt of the NecroDancer — Tier 1
;;;
;;; ldd output confirmed these are the missing libs for the x64 binary.
;;; All are current Guix versions; no pinning required.

(define-public gog-crypt-of-the-necrodancer
  (make-game-launcher
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
  (make-game-launcher
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

;;; Wizard of Legend — Tier 1
;;;
;;; Unity 5-era game (2017).  Binary dlopen()s X11/OpenGL at runtime;
;;; ScreenSelector.so (GTK2 dialog) preloads before the game window opens.
;;; No bundled graphics libs — all X11/Mesa must come from Guix store.

(define-public gog-wizard-of-legend
  (make-game-launcher
   "wizard-of-legend"
   "GOG Games/Wizard of Legend/game"
   "WizardOfLegend.x86_64"
   (list mesa
         libx11
         libxrandr
         libxfixes
         libxcursor
         libxi
         libxinerama
         libxxf86vm
         libxext
         libxscrnsaver
         openal
         gtk+-2
         `(,gcc "lib"))
   #:desktop-name "Wizard of Legend"
   #:desktop-icon "~/GOG Games/Wizard of Legend/support/icon.png"))

;;; Slay the Spire — Tier 1
;;;
;;; packr-wrapped libGDX / LWJGL2 game with bundled JRE (Java 8).
;;; GOG's start.sh is broken — it calls jre/bin/java with no arguments.
;;; Correct binary is SlayTheSpire (packr launcher) which reads config.json,
;;; dlopen()s jre/lib/amd64/server/libjvm.so, then runs the jar via JNI.
;;; liblwjgl64.so (extracted from desktop-1.0.jar at runtime) links against
;;; libjawt.so from the bundled JRE, so jre/lib/amd64 must be on LD_LIBRARY_PATH.
;;; libopenal64.so is bundled and self-contained (only needs glibc).
;;; Audio: pipewire (via libopenal64.so's internal PulseAudio/PipeWire backend).

(define-public gog-slay-the-spire
  (make-game-launcher
   "slay-the-spire"
   "GOG Games/Slay the Spire/game"
   "SlayTheSpire"
   (list mesa
         libx11
         libxext
         libxcursor
         libxrandr
         libxxf86vm
         libxtst
         libxi
         libxrender
         pipewire
         `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}/jre/lib/amd64"
                      "${GAMEDIR}/jre/lib/amd64/server")
   #:desktop-name "Slay the Spire"
   #:desktop-icon "~/GOG Games/Slay the Spire/support/icon.png"))

;;; Death Road to Canada — Tier 1

(define-public gog-death-road-to-canada
  (make-game-launcher
   "death-road-to-canada"
   "GOG Games/Death Road to Canada/game"
   "prog-linux-GOG"
   (list glu
         mesa
         sdl2
         sdl2-mixer
         `(,gcc "lib"))
   #:desktop-name "Death Road To Canada"
   #:desktop-icon "~/GOG Games/Death Road to Canada/support/icon.png"))

;;; ── Direct-download games ────────────────────────────────────────────────

;;; Caves of Qud — Tier 1
;;;
;;; Modern Unity game (UnityPlayer.so separate shared lib, MonoBleedingEdge).
;;; Installed at ~/Games/CavesOfQud (direct download, no GOG installer).
;;; UnityPlayer.so dlopen()s X11/OpenGL/audio at runtime — no bundled libs.
;;; libdecor-0.so.0 and libdecor-cairo.so are bundled in the game root and
;;; found automatically via ${GAMEDIR}/lib-less GAMEDIR itself on the path.

(define-public coq-caves-of-qud
  (make-game-launcher
   "caves-of-qud"
   "Games/CavesOfQud"
   "CoQ.x86_64"
   (list mesa
         libx11
         libxrandr
         libxfixes
         libxcursor
         libxi
         libxext
         pipewire
         `(,gcc "lib"))
   ;; Start at 1280x720 — at full 1920x1200 the console renderer hits
   ;; Unity's 65000-vertex mesh limit and crashes on first frame.
   ;; CoQ reads OptionDisplayResolution=Unset and calls Screen.SetResolution
   ;; to native desktop size, overriding Unity PlayerPrefs and -screen-* args.
   ;; Workaround: temporarily switch the X11 display to 1280x720 so CoQ
   ;; detects that as native, then restore on exit.
   #:default-args '("-screen-width" "1280" "-screen-height" "720")
   #:pre-launch
   '("_COQ_DISPLAY=$(xrandr | awk '/ connected/{print $1; exit}')"
     "_COQ_PREV_MODE=$(xrandr | awk '/*/{print $1; exit}')"
     "xrandr --output \"${_COQ_DISPLAY}\" --mode 1280x720")
   #:post-launch
   '("xrandr --output \"${_COQ_DISPLAY}\" --mode \"${_COQ_PREV_MODE}\"")
   #:desktop-name "Caves of Qud"
   #:desktop-icon "applications-games"))

;;; Dwarf Fortress — Tier 1
;;;
;;; Bay 12 Games direct download (free version).  Installed at ~/Games/DwarfFortress.
;;; Bundled libs (libg_src_lib.so, libfmod.so.13, libfmod_plugin.so,
;;; libsdl_mixer_plugin.so) live in the game root, not a lib/ subdir.
;;; run_df already shows the correct pattern: add game root to LD_LIBRARY_PATH.
;;; System deps: sdl2, sdl2-image (for the graphics frontend), gcc:lib.

(define-public bay12-dwarf-fortress
  (make-game-launcher
   "dwarf-fortress"
   "Games/DwarfFortress"
   "dwarfort"
   (list sdl2
         sdl2-image
         `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}")
   #:desktop-name "Dwarf Fortress"
   #:desktop-icon "applications-games"))
