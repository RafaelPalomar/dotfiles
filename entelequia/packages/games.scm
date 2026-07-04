(define-module (entelequia packages games)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages fontutils)  ; freetype
  #:use-module (gnu packages compression) ; zlib, bzip2
  #:use-module (gnu packages xml)         ; expat (libCEGUIExpatParser.so)
  #:use-module (gnu packages linux)      ; util-linux, eudev, libcap
  #:use-module (gnu packages xorg)       ; libx11, libxrandr, libxfixes, etc.
  #:use-module (gnu packages gl)         ; mesa, libglvnd (libOpenGL.so.0)
  #:use-module (gnu packages image)      ; libpng (libpng16.so.16)
  #:use-module (gnu packages luanti)         ; mesa
  #:use-module (gnu packages audio)      ; openal, pulseaudio, alsa-lib
  #:use-module (gnu packages xiph)       ; libogg, libvorbis, opus
  #:use-module (gnu packages gcc)        ; gcc "lib"
  #:use-module (gnu packages sdl)        ; sdl2
  #:use-module (gnu packages gtk)        ; gtk+ (GTK3), gtk+-2 (GTK2)
  #:use-module (gnu packages glib)       ; glib
  #:use-module (gnu packages pulseaudio) ; pulseaudio (libpulse-simple)
  #:use-module (gnu packages java)       ; openjdk17 for Mindustry
  #:use-module (gnu packages gettext)    ; gettext-minimal (netheroes2)
  #:use-module (gnu packages curl)       ; curl (netheroes2 online client)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system luanti)
  ;; Only the launcher generators (plain define*) need exporting; every
  ;; game package below is define-public.  An earlier exhaustive package
  ;; list here had already drifted out of sync — don't reintroduce one.
  #:export (make-game-launcher
            make-game-fhs-launcher
            make-scummvm-launcher
            make-wine-game-launcher
            make-proton-game-launcher))

;;;
;;; Game launcher helpers
;;;
;;; Tiered architecture for running games on Guix:
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
;;;   Tier 4 — make-scummvm-launcher
;;;     Point-and-click adventures via the ScummVM engine.
;;;
;;;   Tier 5 — make-wine-game-launcher
;;;     Windows games via wine-staging in an FHS container.
;;;
;;;   Tier 6 — make-proton-game-launcher
;;;     Windows games needing Proton-GE (e.g. Unity titles where
;;;     wine-staging stubs EnableMouseInPointer); see
;;;     ~/pks/permanent/20260424T223919 for the tier rationale.
;;;

;;; Tier 1/2 — LD_LIBRARY_PATH wrapper

(define* (make-game-launcher launcher-name game-subdir binary lib-inputs
                              #:key (extra-env '())
                                    (extra-lib-dirs '())
                                    (default-args '())
                                    (pre-launch '())
                                    (post-launch '())
                                    (terminal? #f)
                                    (desktop-name launcher-name)
                                    (desktop-icon "applications-games"))
  "Return a package that installs a shell wrapper under bin/LAUNCHER-NAME.

The wrapper sets LD_LIBRARY_PATH to:
  $GAMEDIR/lib (bundled game libs, highest priority)
  $GAMEDIR/lib64
  EXTRA-LIB-DIRS (shell expressions evaluated at run time,
                  e.g. \"${GAMEDIR}/jre/lib/amd64\")
  the inherited LD_LIBRARY_PATH (preserved, inserted here so that on an
                  NVIDIA host the home-profile libglvnd dispatch set globally
                  by common-home-services wins over a bundled Mesa store path;
                  empty/unset on AMD/Intel, contributing nothing)
  + /gnu/store paths for each package in LIB-INPUTS that has a /lib dir

Also installs a .desktop file so the game appears in rofi/app menus.

GAME-SUBDIR is relative to $HOME (e.g. \"GOG Games/Foo/game\" or \"Games/Bar\").
BINARY is the executable name inside GAME-SUBDIR.
EXTRA-ENV is an alist of (\"VAR\" . \"VALUE\") environment variables.
EXTRA-LIB-DIRS is a list of shell path strings appended after lib64.
DEFAULT-ARGS is a list of strings prepended before \"$@\" in the exec line
  and baked into the .desktop Exec= field (e.g. '(\"-screen-width\" \"1280\")).
TERMINAL? when true makes the wrapper re-exec itself inside kitty if launched
  without a controlling terminal (e.g. from rofi / a .desktop entry), so a
  console app such as a dedicated server shows its output and can be stopped.
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
               ;; Console games (e.g. dedicated servers) re-exec inside a
               ;; terminal when launched without one (rofi/.desktop), so their
               ;; output is visible and the window can be closed to stop them.
               ,@(if terminal?
                     '((format port "if [ -z \"$GAME_LAUNCHER_IN_TERM\" ] && [ ! -t 1 ]; then~%")
                       (format port "  export GAME_LAUNCHER_IN_TERM=1~%")
                       (format port "  exec kitty -- \"$0\" \"$@\"~%")
                       (format port "fi~%"))
                     '())
               (format port "GAMEDIR=\"${HOME}/~a\"~%" ,game-subdir)
               (format port "export LD_LIBRARY_PATH=\"${GAMEDIR}/lib:${GAMEDIR}/lib64")
               ,@(map (lambda (d) `(format port ":~a" ,d)) extra-lib-dirs)
               (format port "${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}")
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

;;; Tier 4 — ScummVM-backed launcher
;;;
;;; For classic adventure games whose data files are handled by ScummVM.
;;; Emits a wrapper that invokes `scummvm -p $GAMEDIR <target>`, relying on
;;; scummvm being on PATH (present in gaming-home-packages).  The game data
;;; lives in GAME-SUBDIR relative to $HOME; SCUMMVM-TARGET is the short
;;; engine-prefixed game ID (e.g. "gob1", "gob2", "gob3").

(define* (make-scummvm-launcher launcher-name game-subdir scummvm-target
                                 #:key (desktop-name launcher-name)
                                       (desktop-icon "applications-games")
                                       (extra-args '()))
  "Return a package that installs a wrapper under bin/LAUNCHER-NAME which
invokes scummvm on the data in ~/GAME-SUBDIR, launching SCUMMVM-TARGET."
  (package
    (name launcher-name)
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils) (ice-9 format))
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (launcher (string-append bin "/" ,launcher-name)))
           (mkdir-p bin)
           (call-with-output-file launcher
             (lambda (port)
               (format port "#!/bin/sh~%")
               (format port "# ScummVM launcher: ~a (target ~a)~%"
                       ,launcher-name ,scummvm-target)
               (format port "GAMEDIR=\"${HOME}/~a\"~%" ,game-subdir)
               (format port "exec scummvm -p \"${GAMEDIR}\"")
               ,@(map (lambda (a) `(format port " ~a" ,a)) extra-args)
               (format port " ~a \"$@\"~%" ,scummvm-target)))
           (chmod launcher #o755)
           (let* ((apps    (string-append out "/share/applications"))
                  (desktop (string-append apps "/" ,launcher-name ".desktop")))
             (mkdir-p apps)
             (call-with-output-file desktop
               (lambda (port)
                 (format port "[Desktop Entry]~%")
                 (format port "Version=1.0~%")
                 (format port "Type=Application~%")
                 (format port "Name=~a~%" ,desktop-name)
                 (format port "Exec=~a~%" ,launcher-name)
                 (format port "Icon=~a~%" ,desktop-icon)
                 (format port "Categories=Game;AdventureGame;~%")
                 (format port "Terminal=false~%"))))))))
    (inputs '())
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "ScummVM launcher for " launcher-name))
    (description
     (string-append
      "Wrapper that invokes scummvm on the game data in ~/"
      game-subdir
      ".  Requires scummvm on PATH (provided by gaming-home-packages)."))
    (home-page "https://www.gnu.org/software/guix/")
    (license license:expat)))

;;; Tier 5 — Wine launcher for Windows-only games (or native Linux
;;; builds that don't render on current Mesa/driver combos).
;;;
;;; Emits a wrapper that sets WINEPREFIX and invokes `wine <EXE>`,
;;; relying on wine64-staging being on PATH (provided via
;;; gaming-home-packages).  The prefix is expected to be set up
;;; once by the user with a one-shot setup script that runs
;;; `wineboot --init` and extracts the game; the launcher only
;;; invokes the already-installed .exe.
;;;
;;; PREFIX-SUBDIR is relative to $HOME (e.g. ".wine-coq").
;;; EXE-RELPATH is relative to $WINEPREFIX/drive_c/ (e.g.
;;;   "CavesOfQud/CoQ.exe").

(define* (make-wine-game-launcher launcher-name prefix-subdir exe-relpath
                                   #:key (desktop-name launcher-name)
                                         (desktop-icon "applications-games")
                                         (extra-env '())
                                         (extra-args '()))
  "Return a package that installs a wrapper invoking `wine <EXE>`
with WINEPREFIX set to ~/PREFIX-SUBDIR, running ~/PREFIX-SUBDIR/drive_c/EXE-RELPATH."
  (package
    (name launcher-name)
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils) (ice-9 format))
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (launcher (string-append bin "/" ,launcher-name)))
           (mkdir-p bin)
           (call-with-output-file launcher
             (lambda (port)
               (format port "#!/bin/sh~%")
               (format port "# Wine launcher: ~a~%" ,launcher-name)
               (format port "export WINEPREFIX=\"${HOME}/~a\"~%" ,prefix-subdir)
               (format port "EXE=\"${WINEPREFIX}/drive_c/~a\"~%" ,exe-relpath)
               (format port "if [ ! -f \"${EXE}\" ]; then~%")
               (format port "  echo \"!! ~a not found at ${EXE}\" >&2~%"
                       ,exe-relpath)
               (format port "  echo \"   Run the one-shot wine setup for this game first.\" >&2~%")
               (format port "  exit 1~%")
               (format port "fi~%")
               ,@(map (lambda (pair)
                        `(format port "export ~a=\"~a\"~%"
                                 ,(car pair) ,(cdr pair)))
                      extra-env)
               (format port "cd \"$(dirname \"${EXE}\")\"~%")
               (format port "exec wine \"${EXE}\"")
               ,@(map (lambda (a) `(format port " ~a" ,a)) extra-args)
               (format port " \"$@\"~%")))
           (chmod launcher #o755)
           (let* ((apps    (string-append out "/share/applications"))
                  (desktop (string-append apps "/" ,launcher-name ".desktop")))
             (mkdir-p apps)
             (call-with-output-file desktop
               (lambda (port)
                 (format port "[Desktop Entry]~%")
                 (format port "Version=1.0~%")
                 (format port "Type=Application~%")
                 (format port "Name=~a~%" ,desktop-name)
                 (format port "Exec=~a~%" ,launcher-name)
                 (format port "Icon=~a~%" ,desktop-icon)
                 (format port "Categories=Game;~%")
                 (format port "Terminal=false~%"))))))))
    (inputs '())
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "Wine launcher for " launcher-name))
    (description
     (string-append
      "Wrapper that invokes wine on the Windows build of "
      launcher-name ".  Requires wine64-staging on PATH (provided by "
      "gaming-home-packages) and a pre-initialised prefix at ~/"
      prefix-subdir "."))
    (home-page "https://www.gnu.org/software/guix/")
    (license license:expat)))

;;; Tier 6 — Proton-GE launcher for Unity / modern-Windows games
;;;
;;; Some Windows titles (notably Unity 6 / IL2CPP builds that use the new
;;; Input System) call Win32 `EnableMouseInPointer`, which upstream wine
;;; stubs as ERROR_CALL_NOT_IMPLEMENTED — Unity then falls back to
;;; Windows.Gaming.Input which does not handle mouse, and clicks never
;;; reach the game even though cursor movement and keyboard work.
;;;
;;; Proton-GE patches these APIs, so it is the right compat layer for
;;; this class of game.  Proton is distributed as a prebuilt tarball of
;;; ELF binaries expecting /lib64/ld-linux-x86-64.so.2 et al — we ship
;;; it unchanged into the store and satisfy the runtime expectations at
;;; launch time with `guix shell --container --emulate-fhs`.

;;; Channels file pinned to guix + nonguix only — used by the launcher's
;;; `guix time-machine -C ...` invocation so the inferior guix has nongnu
;;; on its load path.  We can't use the full channels-lock.scm because
;;; guix-xlibre's pinned commit was rebased away on codeberg (any user
;;; who hasn't already cached it can no longer fetch).  guix + nonguix
;;; are sufficient for the proton FHS profile.
;;;
;;; The store path of this file is baked into the launcher script.

(define proton-fhs-channels
  (plain-file "proton-fhs-channels.scm"
              ;; NOTE: keep the guix commit pin recent enough to bundle Guile
              ;; >= 3.0.10.  The earlier pin 6a483ed7… still bundled Guile 3.0.9
              ;; while its own guix/status.scm already used
              ;; `make-custom-binary-output-port' (a 3.0.10+ symbol), making the
              ;; inferior internally inconsistent on hosts that had to rebuild it
              ;; fresh (curie had cached pre-bumped store paths, kid hosts did
              ;; not).  Bumped 2026-05-31 to match channels-lock.scm.
              "(use-modules (guix channels))
(list (channel
        (name 'guix)
        (url \"https://codeberg.org/guix/guix.git\")
        (branch \"master\")
        (commit \"21898c0a1aae913fe732ad81f01328e34acb5721\")
        (introduction
          (make-channel-introduction
            \"9edb3f66fd807b096b48283debdcddccfea34bad\"
            (openpgp-fingerprint
              \"BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA\"))))
      (channel
        (name 'nonguix)
        (url \"https://gitlab.com/nonguix/nonguix\")
        (branch \"master\")
        (commit \"a8326a5b325400f25c0520c8ef9127fff6d4796d\")
        (introduction
          (make-channel-introduction
            \"897c1a470da759236cc11798f4e0a5f7d4d59fbc\"
            (openpgp-fingerprint
              \"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5\")))))
"))

;;; Manifest baked into the launcher: same package list the inline `guix
;;; shell` had before, plus `nvidia-driver` (which carries libglvnd as a
;;; propagated input).  `time-machine -C proton-fhs-channels -- shell -m
;;; <this-manifest>` gives a profile that puts nvidia libs at
;;; /usr/lib/libGL.so.1 etc. inside the FHS container — the missing
;;; piece that LD_LIBRARY_PATH alone could not provide.

(define proton-fhs-manifest
  (plain-file "proton-fhs-manifest.scm"
              "(use-modules (gnu packages gl)
             (gnu packages vulkan)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages linux)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages commencement)
             (gnu packages fontutils)
             (gnu packages audio)
             (gnu packages pulseaudio)
             (gnu packages python)
             (nongnu packages nvidia))
(packages->manifest
 (list mesa vulkan-loader
       libx11 libxcursor libxrandr libxi libxext libxrender
       libxfixes libxcomposite libxdamage libxxf86vm libxkbcommon
       gcc-toolchain freetype fontconfig
       alsa-lib pulseaudio eudev
       bash coreutils grep sed findutils which
       python python-wrapper
       nvidia-driver))
"))

(define-public proton-ge-10-34
  (package
    (name "proton-ge")
    (version "10-34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/GloriousEggroll/proton-ge-custom/releases/"
             "download/GE-Proton10-34/GE-Proton10-34.tar.gz"))
       (sha256
        (base32 "0gbpipk3x7hqslp2y2h4aiv1jmxcxqbhf3z0iycp6g43dav81iai"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/proton-ge/"))
       #:phases
       (modify-phases %standard-phases
         ;; Proton ships prebuilt ELFs with /lib64/ld-linux-x86-64.so.2 as
         ;; interpreter; none of these Guix-side post-processing phases
         ;; should touch them — they run inside a FHS container at launch.
         (delete 'strip)
         (delete 'validate-runpath)
         (delete 'patch-shebangs))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/GloriousEggroll/proton-ge-custom")
    (synopsis "GloriousEggroll's custom Proton build (prebuilt tarball)")
    (description
     "Proton-GE is a community-patched Proton (wine + DXVK + vkd3d +
various game fixes) distributed as a prebuilt tarball.  This package
ships the unmodified tarball contents under share/proton-ge/; games
are launched via @code{make-proton-game-launcher}, which wraps the
invocation in @code{guix shell --container --emulate-fhs} to provide
the prebuilt binaries with the standard Linux runtime layout they
expect.")
    ;; Proton-GE is distributed under multiple licenses (LGPL for wine,
    ;; various for bundled patches).  Mark as a mix rather than trying to
    ;; enumerate.
    (license (list license:lgpl2.1+ license:expat))))

(define* (make-proton-game-launcher launcher-name game-subdir exe-relpath
                                     #:key
                                       (compat-subdir
                                        (string-append "Games/proton-prefixes/"
                                                       launcher-name))
                                       (desktop-name launcher-name)
                                       (desktop-icon "applications-games")
                                       (extra-env '())
                                       (extra-args '())
                                       (winefsync 0)
                                       (wineesync 0)
                                       (gpu-boost? #f))
  "Return a package that installs a wrapper under bin/LAUNCHER-NAME
which runs ~/GAME-SUBDIR/EXE-RELPATH under Proton-GE inside a
guix-shell FHS container.

GAME-SUBDIR is the directory containing the extracted Windows game,
relative to $HOME (e.g. \"Games/9Kings\").
EXE-RELPATH is the .exe relative to GAME-SUBDIR (e.g. \"9Kings.exe\").
COMPAT-SUBDIR is Proton's per-game compat data dir, relative to $HOME;
Proton creates <COMPAT-SUBDIR>/pfx/ on first launch.

The generated script references a pinned Proton-GE store path and
invokes @code{guix shell --container --emulate-fhs} with the runtime
deps wine/Proton pulls in (libX*, mesa, vulkan-loader, gcc:lib for
libgcc_s, freetype, fontconfig, ALSA/Pulse, python3).  Sync primitives
fsync/esync are disabled; Proton falls back to wineserver-sync, which
is adequate for single-player titles and sidesteps shm/memfd setup
inside the container."
  (package
    (name launcher-name)
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils) (ice-9 format))
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (launcher (string-append bin "/" ,launcher-name))
                (proton   (assoc-ref %build-inputs "proton-ge"))
                (channels (assoc-ref %build-inputs "channels"))
                (manifest (assoc-ref %build-inputs "manifest")))
           (mkdir-p bin)
           (call-with-output-file launcher
             (lambda (port)
               (format port "#!/bin/sh~%")
               (format port "# Proton-GE launcher: ~a~%" ,launcher-name)
               (format port "set -e~%")
               (format port "GAMEDIR=\"${HOME}/~a\"~%" ,game-subdir)
               (format port "EXE=\"${GAMEDIR}/~a\"~%" ,exe-relpath)
               (format port "COMPAT=\"${HOME}/~a\"~%" ,compat-subdir)
               (format port "if [ ! -f \"${EXE}\" ]; then~%")
               (format port "  echo \"!! ~a not found at ${EXE}\" >&2~%"
                       ,exe-relpath)
               (format port "  echo \"   Install the game files into ${GAMEDIR} first.\" >&2~%")
               (format port "  exit 1~%")
               (format port "fi~%")
               (format port "mkdir -p \"${COMPAT}\" \"${HOME}/.steam/steam\"~%")
               ;; Optional GPU boost (#:gpu-boost? #t): write "high" to
               ;; /sys/class/drm/card0/device/power_dpm_force_performance_level
               ;; before launching the game, and revert to "auto" on
               ;; EXIT/INT/TERM (or game crash).  Requires the boot-time
               ;; perms service to have made the file group-writable to
               ;; the user (see curie.scm 'amd-gpu-perf-perms').
               ;;
               ;; Safer than `power_dpm_force_performance_level=high` baked
               ;; into a kernel arg or boot service: GPU only runs at max
               ;; clock for the duration of the game, then returns to auto.
               ;; Mirrors feral-interactive's gamemoded behaviour.
               ,@(if gpu-boost?
                     '((format port "GPU_PERF_FILE=/sys/class/drm/card0/device/power_dpm_force_performance_level~%")
                       (format port "if [ -w \"$GPU_PERF_FILE\" ]; then~%")
                       (format port "  GPU_PERF_PREV=$(cat \"$GPU_PERF_FILE\")~%")
                       (format port "  trap 'echo \"$GPU_PERF_PREV\" > \"$GPU_PERF_FILE\" 2>/dev/null || true' EXIT INT TERM HUP~%")
                       (format port "  echo high > \"$GPU_PERF_FILE\"~%")
                       (format port "  echo \"[gpu-boost] $GPU_PERF_FILE: $GPU_PERF_PREV -> high (will revert on exit)\" >&2~%")
                       (format port "else~%")
                       (format port "  echo \"[gpu-boost] $GPU_PERF_FILE not writable; skipping boost\" >&2~%")
                       (format port "fi~%"))
                     '())
               ;; NVIDIA passthrough via baked manifest + time-machine.
               ;; nvidia-driver lives in nonguix; system guix doesn't
               ;; carry that channel.  We ship a 2-channel
               ;; (proton-fhs-channels) and a manifest including
               ;; nvidia-driver alongside mesa+wine-deps.  At runtime
               ;; `guix time-machine -C channels -- shell -m manifest`
               ;; produces an FHS profile where /usr/lib/libGL.so.1
               ;; resolves to nvidia (via libglvnd dispatch) on hosts
               ;; with /dev/nvidia0, mesa otherwise.
               ;;
               ;; First-run cost on a fresh user: ~10 min while
               ;; time-machine fetches the channels and builds the
               ;; inferior guix.  Cached after that — subsequent
               ;; launches are instant.  A pre-warm hook in the deploy
               ;; would eliminate even that first hit.
               (format port "CHANNELS=\"~a\"~%" channels)
               (format port "MANIFEST=\"~a\"~%" manifest)
               (display "GPU_EXPOSE=\"\"\n" port)
               (display "if [ -e /dev/nvidia0 ]; then\n" port)
               (display "  for dev in /dev/nvidia0 /dev/nvidiactl /dev/nvidia-modeset /dev/nvidia-uvm /dev/nvidia-uvm-tools; do\n" port)
               (display "    [ -e \"$dev\" ] && GPU_EXPOSE=\"$GPU_EXPOSE --expose=$dev\"\n" port)
               (display "  done\n" port)
               (display "fi\n" port)
               (display "exec /run/current-system/profile/bin/guix time-machine -C \"$CHANNELS\" -- \\\n" port)
               (display "  shell --container --emulate-fhs --network \\\n" port)
               (display "  --preserve='^DISPLAY$' \\\n" port)
               (display "  --preserve='^XAUTHORITY$' \\\n" port)
               (display "  --preserve='^DBUS_SESSION_BUS_ADDRESS$' \\\n" port)
               (display "  --preserve='^XDG_RUNTIME_DIR$' \\\n" port)
               (display "  --preserve='^PULSE' \\\n" port)
               (display "  --share=\"${HOME}\" \\\n" port)
               (display "  --share=/tmp \\\n" port)
               (display "  --share=/dev/shm \\\n" port)
               (display "  --share=\"/run/user/$(id -u)\" \\\n" port)
               (display "  --expose=/dev/dri \\\n" port)
               (display "  --expose=/dev/input \\\n" port)
               (display "  --expose=/dev/snd \\\n" port)
               (display "  --expose=/sys \\\n" port)
               (display "  $GPU_EXPOSE \\\n" port)
               ;; Proton lives in the store; expose so the container
               ;; sees it (container namespaces /gnu/store).
               (format port "  --expose=~a \\\n" proton)
               (display "  -m \"$MANIFEST\" \\\n" port)
               (format port "  -- sh -c '\\\n")
               (format port "      export STEAM_COMPAT_CLIENT_INSTALL_PATH=\"$HOME/.steam/steam\";\\\n")
               (format port "      export STEAM_COMPAT_DATA_PATH=\"$HOME/~a\";\\\n"
                       ,compat-subdir)
               (format port "      export WINEFSYNC=~a;\\\n" ,winefsync)
               (format port "      export WINEESYNC=~a;\\\n" ,wineesync)
               ;; Per-game extra env vars (RADV_PERFTEST, DXVK_ASYNC,
               ;; mesa_glthread, etc.).  Exported INSIDE the FHS
               ;; container's sh -c body so they survive the `guix shell
               ;; --container` namespace transition.
               ,@(map (lambda (pair)
                        `(format port "      export ~a=~s;\\\n"
                                 ,(car pair) ,(cdr pair)))
                      extra-env)
               ;; Disable Xalia (Proton's accessibility/SDL helper) — its
               ;; SDL_VideoInit fails inside the Guix FHS container with
               ;; "Video driver  not supported" and aborts the launch.
               ;; The game itself doesn't need Xalia; it only matters for
               ;; Steam Deck on-screen-keyboard hints.
               (format port "      export PROTON_USE_XALIA=0;\\\n")
               ;; NVIDIA: tell libglvnd to dispatch to the NVIDIA
               ;; backend.  The manifest already places nvidia-driver
               ;; libs at /usr/lib via the FHS profile; libglvnd's
               ;; libGL.so.1 picks the vendor based on this env var
               ;; (and the GLX context's screen vendor info).
               (format port "      if [ -e /dev/nvidia0 ]; then export __GLX_VENDOR_LIBRARY_NAME=nvidia; fi;\\\n")
               ;; Point wine's winepulse.drv at PipeWire's Pulse compat
               ;; socket; otherwise ALSA is tried first and fails for lack
               ;; of /dev/snd card probing inside the container.
               (format port "      export PULSE_SERVER=\"unix:$XDG_RUNTIME_DIR/pulse/native\";\\\n")
               (format port "      export PULSE_RUNTIME_PATH=\"$XDG_RUNTIME_DIR/pulse\";\\\n")
               (format port "      cd \"$HOME/~a\";\\\n" ,game-subdir)
               (format port "      exec python3 ~a/share/proton-ge/proton run \"./~a\"" proton ,exe-relpath)
               ,@(map (lambda (a) `(format port " ~a" ,a)) extra-args)
               (format port " \"$@\"'~%")))
           (chmod launcher #o755)
           (let* ((apps    (string-append out "/share/applications"))
                  (desktop (string-append apps "/" ,launcher-name ".desktop")))
             (mkdir-p apps)
             (call-with-output-file desktop
               (lambda (port)
                 (format port "[Desktop Entry]~%")
                 (format port "Version=1.0~%")
                 (format port "Type=Application~%")
                 (format port "Name=~a~%" ,desktop-name)
                 (format port "Exec=~a~%" ,launcher-name)
                 (format port "Icon=~a~%" ,desktop-icon)
                 (format port "Categories=Game;~%")
                 (format port "Terminal=false~%"))))))))
    (inputs
     `(("proton-ge" ,proton-ge-10-34)
       ;; Baked into the launcher's script: minimal channels-lock and
       ;; full FHS manifest including nvidia-driver.  Both live in
       ;; /gnu/store on every machine that installs the launcher
       ;; (curie included — small files, KB scale).  The actual
       ;; nvidia-driver package is fetched lazily on first launch via
       ;; time-machine; AMD-only hosts never trigger that fetch.
       ("channels" ,proton-fhs-channels)
       ("manifest" ,proton-fhs-manifest)))
    (supported-systems '("x86_64-linux"))
    (synopsis (string-append "Proton-GE launcher for " launcher-name))
    (description
     (string-append
      "Wrapper that runs " launcher-name " under Proton-GE inside a "
      "'guix shell --container --emulate-fhs' environment.  Proton creates "
      "its compat prefix at ~/" compat-subdir "/pfx/ on first launch.  The "
      "game's extracted Windows files are expected at ~/" game-subdir "."))
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

;;; Starbound — Tier 1 (native Linux)
;;;
;;; Native Linux GOG build (1.4.4).  Binary is at game/linux/starbound;
;;; bundled libsteam_api.so lives in that same dir so we add it to
;;; LD_LIBRARY_PATH via extra-lib-dirs.  External deps: SDL2 (window/input),
;;; mesa (libGL.so.1), glu (libGLU.so.1), gcc:lib (libstdc++/libgcc_s).
;;;
;;; Install:
;;;   binwalk shows the .sh contains a ZIP at offset 0x9D7A1 (645025).
;;;   dd if=starbound_1_4_4_34261.sh of=payload.zip bs=1M iflag=skip_bytes \
;;;     skip=645025
;;;   unzip payload.zip -d /tmp/sb && mv /tmp/sb/data/noarch ~/GOG\ Games/Starbound
;;;   patchelf --set-interpreter \
;;;     $(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2) \
;;;     ~/GOG\ Games/Starbound/game/linux/starbound

(define-public gog-starbound
  (make-game-launcher
   "starbound"
   "GOG Games/Starbound/game/linux"
   "starbound"
   (list sdl2
         mesa
         glu
         `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}")
   #:desktop-name "Starbound"
   #:desktop-icon "~/GOG Games/Starbound/support/icon.png"))

;;; Starbound dedicated server — Tier 1 (native Linux)
;;;
;;; The single-player client never opens a network port (its embedded server
;;; talks over an internal pipe), so multiplayer REQUIRES the dedicated
;;; starbound_server, which binds 0.0.0.0:21025.  Same install dir as the
;;; client; binary is game/linux/starbound_server.  ldd resolves entirely to
;;; Guix glibc (no SDL/mesa/GL, not even libstdc++ — the server is headless),
;;; so the only lib dir needed is ${GAMEDIR} for the dlopen'd libsteam_api.so.
;;;
;;; Like every Tier-1 game here, the binary needs its interpreter patched once
;;; (the GOG build ships /lib64/ld-linux-x86-64.so.2) and execute permission:
;;;   chmod +x ~/GOG\ Games/Starbound/game/linux/starbound_server
;;;   patchelf --set-interpreter \
;;;     $(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2) \
;;;     ~/GOG\ Games/Starbound/game/linux/starbound_server
;;;
;;; The host quits single-player first (server + client lock the same
;;; storage/universe).  Players then join via the client's multiplayer toggle:
;;; the host at 127.0.0.1, LAN peers at the host's IP, port 21025.

(define-public gog-starbound-server
  (make-game-launcher
   "starbound-server"
   "GOG Games/Starbound/game/linux"
   "starbound_server"
   (list `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}")
   #:terminal? #t
   #:desktop-name "Starbound (Dedicated Server)"
   #:desktop-icon "~/GOG Games/Starbound/support/icon.png"))

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

;;; Torchlight 2 — Tier 1
;;;
;;; Ogre3D-based action RPG.  Bundled libs in lib64/: SDL2, Ogre, CEGUI,
;;; fmod, freetype — covered by $GAMEDIR/lib64 on LD_LIBRARY_PATH.
;;; Missing system libs: libGL (mesa), libGLU (glu), libSM/libICE (libsm),
;;; libuuid (util-linux:lib), libz/libbz2 (zlib/bzip2),
;;; libstdc++/libgcc_s (gcc:lib).
;;; Audio: fmod dlopen()s libasound.so.2 (ALSA) and libpulse-simple.so.0
;;; (PulseAudio); pulseaudio provides libpulse-simple, alsa-lib covers ALSA.
;;; CEGUI XML: libCEGUIExpatParser.so dlopen()s libexpat.so.1 at runtime;
;;; expat must be explicit because NVIDIA does not pull it in (mesa does not
;;; either, but curie happened to get it transitively via pulseaudio).

(define-public gog-torchlight-2
  (make-game-launcher
   "torchlight-2"
   "GOG Games/Torchlight 2/game"
   "Torchlight2.bin.x86_64"
   (list mesa
         glu
         libsm
         libice
         pulseaudio
         alsa-lib
         expat
         `(,util-linux "lib")
         zlib
         bzip2
         `(,gcc "lib"))
   ;; TL2 enumerates characters by opening "save" relative to CWD (the game
   ;; dir).  Ensure a symlink exists so it finds ~/.local/share/Runic Games/…
   #:pre-launch
   '("SAVEDIR=\"${HOME}/.local/share/Runic Games/Torchlight 2/save\""
     "mkdir -p \"${SAVEDIR}\""
     "if [ ! -e \"${GAMEDIR}/save\" ]; then"
     "  ln -sf \"${SAVEDIR}\" \"${GAMEDIR}/save\""
     "fi")
   #:desktop-name "Torchlight 2"
   #:desktop-icon "~/GOG Games/Torchlight 2/support/icon.png"))

;;; Duskers — Tier 1
;;;
;;; Unity 5-era game (2016).  ScreenSelector.so (GTK2 dialog) appears first;
;;; main binary needs mesa + X11 libs (libGL, libX11, libXcursor, libXrandr).
;;; Audio: FMOD engine embedded in the binary dlopen()s libasound.so.2 and
;;; libpulse-simple.so.0 at runtime — alsa-lib + pulseaudio provide both.
;;; Same engine/plugin pattern as Wizard of Legend with FMOD audio bolted on.
;;; eudev is mandatory: Unity dlopen()s libudev.so for gamepad hotplug, and
;;; without it the game hangs silently after the joystick-config parse phase
;;; (no window ever maps, process stalls in udev enumeration).

(define-public gog-duskers
  (make-game-launcher
   "duskers"
   "GOG Games/Duskers/game"
   "Duskers_linux.x86_64"
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
         gtk+-2
         alsa-lib
         pulseaudio
         eudev
         `(,gcc "lib"))
   #:desktop-name "Duskers"
   #:desktop-icon "~/GOG Games/Duskers/support/icon.png"))

;;; Papers, Please — Tier 1
;;;
;;; Modern Unity game (UnityPlayer.so + GameAssembly.so in the game dir).
;;; ldd on the main binary only flags libgcc_s.so.1; UnityPlayer.so dlopen()s
;;; X11/mesa/audio at runtime.  Bundled libs (UnityPlayer.so, GameAssembly.so)
;;; sit next to the binary so ${GAMEDIR} must be on LD_LIBRARY_PATH.
;;; Audio: UnityPlayer.so dlopen()s libasound.so.2 (ALSA) and
;;; libpulse-simple.so.0 (PulseAudio) — alsa-lib + pulseaudio are required.
;;; pipewire-pulse on the host translates libpulse-simple calls transparently.

(define-public gog-papers-please
  (make-game-launcher
   "papers-please"
   "GOG Games/Papers Please/game"
   "PapersPlease"
   (list mesa
         libx11
         libxrandr
         libxfixes
         libxcursor
         libxi
         libxext
         alsa-lib
         pulseaudio
         `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}")
   #:desktop-name "Papers, Please"
   #:desktop-icon "~/GOG Games/Papers Please/support/icon.png"))

;;; Gobliiins (1991) — Tier 4 (ScummVM / GOB engine)
;;;
;;; GOG ships only a Windows installer bundling Windows ScummVM.  On Linux
;;; we extract the game data (INTRO.STK, Track1.mp3, GOB.LIC, FDD/) via
;;; innoextract into ~/Games/Gobliiins and run it with the host scummvm.
;;; ScummVM auto-detects 5 language variants (en/de/fr/it/es) — it will
;;; prompt on first launch if no preference is set.

(define-public gog-gobliiins
  (make-scummvm-launcher
   "gobliiins"
   "Games/Gobliiins"
   "gob1"
   #:desktop-name "Gobliiins"))

;;; Gobliins 2 - The Prince Buffoon (1992) — Tier 4 (ScummVM / GOB engine)

(define-public gog-gobliins-2
  (make-scummvm-launcher
   "gobliins-2"
   "Games/Gobliins2"
   "gob2"
   #:desktop-name "Gobliins 2 - The Prince Buffoon"))

;;; Goblins Quest 3 (1993) — Tier 4 (ScummVM / GOB engine)

(define-public gog-goblins-quest-3
  (make-scummvm-launcher
   "goblins-quest-3"
   "Games/GoblinsQuest3"
   "gob3"
   #:desktop-name "Goblins Quest 3"))

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

;;; They Are Billions — Tier 6 (Proton-GE)
;;;
;;; Windows-only on GOG (no Linux build).  SlimDX / DirectX 9 + .NET 4.6
;;; game with GOG Galaxy integration.  wine-staging 10.x trips on two
;;; issues: (a) Mono's DotNetZip can't decrypt TAB's password-protected
;;; .dat files ("game data files corrupted"); (b) Real .NET 4.6+ on a
;;; 32-bit prefix throws BadImageFormatException because GalaxyCSharp
;;; P/Invokes Galaxy64.dll (64-bit).  Proton-GE sidesteps both: its
;;; patched wine-mono handles the encrypted zips, and its protonfix for
;;; Steam AppID 644930 installs gdiplus + WINE_MONO_HIDETYPES=1 which
;;; applies to the GOG version equally since protonfixes matches on
;;; EXE name.
;;;
;;; Install:
;;;   mkdir -p ~/Games/TheyAreBillions
;;;   innoextract -d ~/Games/TheyAreBillions \
;;;     ~/Games/gog-installers/setup_they_are_billions_1.1.4.10_*_64bit*.exe
;;;
;;; Run the 64-bit binary — Galaxy64.dll is 64-bit only.

(define-public gog-they-are-billions
  (make-proton-game-launcher
   "they-are-billions"
   "Games/TheyAreBillions"
   "TheyAreBillions.exe"
   #:desktop-name "They Are Billions"
   #:desktop-icon "applications-games"))

;;; 9 Kings — Tier 6 (Proton-GE)
;;;
;;; Unity 6 / IL2CPP strategy-deckbuilder; Windows-only on GOG.  Upstream
;;; wine-staging 10.0/11.0 stubs EnableMouseInPointer, so Unity's new
;;; Input System falls through to Windows.Gaming.Input for pointer events
;;; and mouse clicks are silently dropped (cursor movement + keyboard
;;; still work — deceptive).  Proton-GE patches this; the game then plays
;;; correctly with mouse + keyboard.
;;;
;;; Setup (one-time, before first launch):
;;;   mkdir -p ~/Games/9Kings
;;;   innoextract -d ~/Games/9Kings \
;;;     ~/Games/gog-installers/9_kings/setup_9_kings_*.exe
;;;
;;; The 9Kings/ root inside the extracted tree contains 9Kings.exe.

(define-public gog-9-kings
  (make-proton-game-launcher
   "9-kings"
   "Games/9Kings"
   "9Kings.exe"
   #:desktop-name "9 Kings"
   #:desktop-icon "applications-games"))

;;; He is Coming — Tier 6 (Proton-GE)
;;;
;;; Unity 2022/IL2CPP roguelike-deckbuilder (Eager Monkey), Windows-only
;;; on GOG.  Bundles Galaxy64.dll + EOSSDK + Firebase — same class of
;;; .NET/Win32 stubs that trip wine-staging on TAB.  Routed through
;;; Tier 6 by default, same as 9 Kings.
;;;
;;; Setup (one-time, before first launch):
;;;   mkdir -p ~/Games/HeIsComing
;;;   innoextract -d ~/Games/HeIsComing \
;;;     ~/Games/gog-installers/setup_he_is_coming_*.exe
;;;
;;; Binary name has spaces: "He is coming.exe" (lowercase 'c').

(define-public gog-he-is-coming
  (make-proton-game-launcher
   "he-is-coming"
   "Games/HeIsComing"
   "He is coming.exe"
   #:desktop-name "He is Coming"
   #:desktop-icon "applications-games"))

;;; No Man's Sky — Tier 6 (Proton-GE)
;;;
;;; Hello Games's procedural-universe explorer.  Windows-only on GOG
;;; (the early native Linux build was dropped).  DirectX 11 / Vulkan;
;;; Proton-GE handles both via dxvk + vkd3d.  Bundles GOG Galaxy SDK
;;; (Galaxy.dll / Galaxy64.dll), same .NET-on-wine-mono surface that
;;; TAB / 9 Kings / He is Coming hit — Proton-GE has the patched
;;; wine-mono + protonfix database we already rely on for those.
;;;
;;; Hardware target: curie (AMD Strix iGPU, Radeon 880M/890M, 30 G
;;; RAM) — comfortably above NMS's GTX 1060 recommendation.  Should
;;; also run on alucard (GTX 1650, Tier 6 NVIDIA path proven).
;;;
;;; Setup (one-time, before first launch):
;;;   mkdir -p ~/Games/NoMansSky
;;;   innoextract -d ~/Games/NoMansSky \
;;;     "~/Games/gog-installers/no_mans_sky/setup_no_mans_sky_6.40_theswarm_*.exe"
;;;
;;; The extracted tree has Binaries/NMS.exe.  PEGI 7.

(define-public gog-no-mans-sky
  (make-proton-game-launcher
   "no-mans-sky"
   "Games/NoMansSky"
   "Binaries/NMS.exe"
   #:desktop-name "No Man's Sky"
   #:desktop-icon "applications-games"
   ;; NMS is graphics + draw-call heavy on Strix iGPU; wine's sync
   ;; primitives matter a lot.  TAB's reason for disabling fsync/esync
   ;; (GOG Galaxy SDK shm/memfd quirks) doesn't apply to NMS — it
   ;; doesn't ship Galaxy.dll on the GOG offline build.
   #:winefsync 1
   #:wineesync 1
   ;; #:gpu-boost? — DISABLED 2026-05-30 on curie (Strix Halo gfx1150).
   ;; Both "permanently force high" and "per-game force high + revert on
   ;; exit" via /sys/.../power_dpm_force_performance_level cause a hard
   ;; power-off on this silicon under NMS workload.  Not a kernel panic
   ;; — full SoC power cut, suggesting VRM trip rather than driver hang.
   ;; Mesa 25.2.3 + kernel 6.18 are still maturing for gfx1150 power
   ;; management; revisit after a Mesa bump.  The boot-time perms
   ;; service in curie.scm is harmless to leave in place; it just makes
   ;; the sysfs file writable so manual `echo profile_standard > ...'
   ;; tests don't need sudo.
   ;; #:gpu-boost? #t
   ;; AMD-specific tuning, validated by NMS community on RADV:
   ;;   RADV_PERFTEST=gpl   — Vulkan Graphics Pipeline Library; cuts
   ;;                         shader-compile stutter dramatically
   ;;   DXVK_ASYNC=1        — async shader compile
   ;;   DXVK_HUD            — left empty by default; set on the command
   ;;                         line for diagnostics (=fps,gpuload)
   ;;   mesa_glthread=true  — multi-threaded GL where DXVK falls back
   #:extra-env '(("RADV_PERFTEST" . "gpl")
                 ("DXVK_ASYNC"    . "1")
                 ("mesa_glthread" . "true"))))

;;; Barony — Tier 1 (native Linux)
;;;
;;; Native Linux GOG build (v5.0.1).  SDL2 roguelike-FPS (Turning Wheel).
;;; The game dir bundles its own libSDL2/libSDL2_image/_net/_ttf, libfmod,
;;; libphysfs, libtheora(player), libpng12 and libz, so ${GAMEDIR}/game on
;;; LD_LIBRARY_PATH (via #:extra-lib-dirs) satisfies those.  Missing system
;;; libs after patchelf'ing barony.x86_64:
;;;   libogg/libvorbis/libopus  → libogg, libvorbis, opus (xiph) — Theora
;;;                               audio + opus codecs the bundled libs link to
;;;   libOpenGL.so.0            → libglvnd (GLVND dispatch; SDL2 GL backend)
;;;   libGL.so.1                → mesa.  The binary ships needing libOpenGL.so.0
;;;                               (pure GLVND), but Guix's mesa is the classic
;;;                               non-GLVND build (no libGLX_mesa.so.0 vendor),
;;;                               so GLVND has no driver to dispatch to → the GL
;;;                               context comes up with vendor/renderer/version
;;;                               all (null), every shader fails to compile, and
;;;                               you get a black screen.  Fix: patchelf the
;;;                               libOpenGL.so.0 NEEDED to mesa's classic
;;;                               libGL.so.1 (which exports the full GL API) so
;;;                               the binary and SDL share one GL stack.  See the
;;;                               one-time install step below.
;;;   libpng16.so.16            → libpng (the bundled libpng12 is for older
;;;                               assets; the binary itself links libpng16)
;;;   libz.so.1                 → zlib.  The game bundles libz.so.1 too, but
;;;                               it tops out at ZLIB_1.2.3.4, while Guix's
;;;                               libpng16 needs ZLIB_1.2.9.  Because ${GAMEDIR}
;;;                               precedes the store dirs on LD_LIBRARY_PATH the
;;;                               stale bundled copy wins, so it MUST be removed
;;;                               from the game dir (one-time, see below).
;;;   libstdc++/libgcc_s        → gcc:lib
;;;   libudev.so.1              → eudev.  The bundled SDL2 dlopen()s libudev for
;;;                               input-device detection ("Could not initialize
;;;                               UDEV" without it).
;;;   libpulse.so.0/libasound.so.2 → pulseaudio + alsa-lib.  The bundled libfmod
;;;                               dlopen()s both for audio output; without them
;;;                               FMOD silently falls back to the "NoSound
;;;                               Driver" (no sound).  pulseaudio's client lib
;;;                               connects to curie's pipewire-pulse server.
;;; mesa + the X11 libs cover SDL2's runtime dlopen() of GLX/X11.
;;;
;;; Install (one-time):
;;;   The GOG installer is a makeself wrapper around a zip with the standard
;;;   data/noarch/ layout.  Extract straight from the .sh with unzip:
;;;     mkdir -p ~/"GOG Games/Barony"
;;;     guix shell unzip -- unzip ~/Games/gog-installers/barony/barony_*_linux_*.sh \
;;;       'data/noarch/*' -d /tmp/barony
;;;     cp -a /tmp/barony/data/noarch/game /tmp/barony/data/noarch/support \
;;;       ~/"GOG Games/Barony"/
;;;     cd ~/"GOG Games/Barony/game"
;;;     guix shell patchelf -- bash -c '\
;;;       for b in barony.x86_64 editor.x86_64; do \
;;;         patchelf --set-interpreter \
;;;           $(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2) \
;;;           --replace-needed libOpenGL.so.0 libGL.so.1 "$b"; \
;;;       done'
;;;   The --replace-needed swap is mandatory (see libGL.so.1 note above) — the
;;;   editor.x86_64 needs it too.  Then drop the stale bundled zlib so Guix's
;;;   libz (ZLIB_1.2.9) is used instead of the bundled ZLIB_1.2.3.4:
;;;     rm ~/"GOG Games/Barony/game/libz.so.1"
;;;
;;; curie-only: registered in home/machines/curie-rafael.scm, NOT in the
;;; shared gaming-home-packages list — so the kids' homes (alucard/hopper)
;;; never receive it.

(define-public gog-barony
  (make-game-launcher
   "barony"
   "GOG Games/Barony/game"
   "barony.x86_64"
   (list mesa
         libpng
         zlib
         eudev
         pulseaudio
         alsa-lib
         libogg
         libvorbis
         opus
         libx11
         libxext
         libxcursor
         libxrandr
         libxi
         libxxf86vm
         libxscrnsaver
         libxinerama
         `(,gcc "lib"))
   #:extra-lib-dirs '("${GAMEDIR}")
   #:desktop-name "Barony"
   #:desktop-icon "~/GOG Games/Barony/support/icon.png"))

;;; ── Direct-download games ────────────────────────────────────────────────

;;; Caves of Qud — Tier 5 (Wine) on curie
;;;
;;; The native Linux build (Unity 2021 LTS, build 2.0.210) does not
;;; render on AMD Radeon gfx1150 (RDNA 3.5) with Mesa 25.2.3 — audio
;;; and game cursor work, but the main framebuffer stays black across
;;; every launcher variant tried (see project_coq_black_screen.md
;;; memory for the full diagnostic log).  The Windows build under
;;; wine-staging 11.0 renders correctly via wined3d (DX11 → OpenGL).
;;;
;;; Setup: one-time `~/.dotfiles/scripts/coq-wine-setup.sh <zip>`
;;; creates the prefix at ~/.wine-coq and extracts the game into
;;; drive_c/CavesOfQud/.  Requires wine64-staging on PATH (present in
;;; gaming-home-packages).
;;;
;;; Revisit: when a future Mesa / Unity / CoQ combination fixes the
;;; native rendering, swap this back to the native Tier 1 launcher —
;;; the broken native config is preserved in git history (tag
;;; coq-native-broken or commit immediately preceding this change).

(define-public coq-caves-of-qud
  (make-wine-game-launcher
   "caves-of-qud"
   ".wine-coq"
   "CavesOfQud/CoQ.exe"
   #:desktop-name "Caves of Qud"
   #:desktop-icon "applications-games"))

;;; Caves of Qud — native Tier 1 variant
;;;
;;; The original Linux binary works on Intel iGPUs (e.g. hopper UHD 620);
;;; the wine variant above was a workaround for curie's gfx1150 + Mesa 25
;;; black-screen issue.  Machines without that issue should prefer this
;;; native launcher (faster, no Wine overhead).
;;;
;;; Bundled libdecor lives in the game dir; $ORIGIN RPATH handles it.

(define-public coq-caves-of-qud-native
  (make-game-launcher
   "caves-of-qud-native"
   "Games/CavesOfQud"
   "CoQ.x86_64"
   (list mesa
         libxinerama libxext libxcursor libxrandr libxxf86vm
         libxtst libxi libxrender libx11
         pipewire
         `(,gcc "lib"))
   #:desktop-name "Caves of Qud (native)"
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

;;; Mindustry — hermetic Java package
;;;
;;; Anuken/Mindustry (free GPL3 release).  Unlike the GOG / Bay 12
;;; launchers (which wrap user-managed install dirs), the jar here is
;;; fetched and pinned by Guix at build time and lives in /gnu/store.
;;; No ~/Games/Mindustry/ setup needed.  Save data still goes to
;;; ~/.local/share/Mindustry (Mindustry's default).
;;;
;;; The jar bundles its LWJGL3 natives (extracted at runtime), so no
;;; LD_LIBRARY_PATH wiring — just a JRE.  Upstream builds against
;;; Java 17+; openjdk17 is the documented minimum.

(define-public anuken-mindustry
  (package
    (name "anuken-mindustry")
    (version "157.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Anuken/Mindustry/releases/download/v"
             version "/Mindustry.jar"))
       (file-name (string-append "Mindustry-" version ".jar"))
       (sha256
        (base32 "0cwzcyys9ycq69ns36fwbc1nvwd9s294rd5hkpkp8nfg21vqjwxb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out      (assoc-ref %outputs "out"))
                (jdk      (assoc-ref %build-inputs "openjdk"))
                (jar-src  (assoc-ref %build-inputs "source"))
                (bin      (string-append out "/bin"))
                (share    (string-append out "/share/mindustry"))
                (apps     (string-append out "/share/applications"))
                (jar      (string-append share "/Mindustry.jar"))
                (launcher (string-append bin "/mindustry"))
                (desktop  (string-append apps "/mindustry.desktop")))
           (mkdir-p bin)
           (mkdir-p share)
           (mkdir-p apps)
           (copy-file jar-src jar)
           (call-with-output-file launcher
             (lambda (port)
               (format port "#!/bin/sh~%")
               (format port "exec \"~a/bin/java\" -jar \"~a\" \"$@\"~%"
                       jdk jar)))
           (chmod launcher #o755)
           (call-with-output-file desktop
             (lambda (port)
               (format port "[Desktop Entry]~%")
               (format port "Version=1.0~%")
               (format port "Type=Application~%")
               (format port "Name=Mindustry~%")
               (format port "Exec=mindustry~%")
               (format port "Icon=applications-games~%")
               (format port "Categories=Game;~%")
               (format port "Terminal=false~%")))))))
    (inputs `(("openjdk" ,openjdk17 "jdk")))
    (supported-systems '("x86_64-linux"))
    (synopsis "Mindustry — tower defense / factory game (Anuken)")
    (description
     "Mindustry is a free, GPL3-licensed tower-defense / factory game
by Anuken.  This package fetches the official Mindustry.jar release
asset, pins it by SHA-256, and provides a @command{mindustry} launcher
that runs it with the Guix-managed openjdk JRE.")
    (home-page "https://mindustrygame.github.io/")
    (license license:gpl3)))


(define-public luanti-mobs-goblins
  (package
    (name "luanti-mobs-goblins")
    ;; Upstream does not use version numbers, so use the release title
    ;; from ContentDB instead;
    (version "2021-11-14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/freelikegnu/goblins")
             (commit "ce27b15f87452c9614b515b8a9b53af5d0e8e276")))
       (sha256
        (base32 "094mm9gid07xwb5da1anp5cnfq74sqrmbdl2mh8yl8pn0c5h9daj"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-mineshaft-nil-check
           ;; mcl_structures.registered_structures["mineshaft"] may be nil in
           ;; some Mineclonia/VoxeLibre versions.  Guard the existing condition.
           (lambda _
             (substitute* "terrain.lua"
               (("goblins\\.compat_mode == \"mc2\" and goblins_lair_rail_corridor_chance ~= 0 and goblins_lair_chance ~= 0")
                "goblins.compat_mode == \"mc2\" and goblins_lair_rail_corridor_chance ~= 0 and goblins_lair_chance ~= 0 and mcl_structures.registered_structures[\"mineshaft\"]"))))
         (add-after 'fix-mineshaft-nil-check 'lower-dungeon-count-threshold
           ;; Upstream goblins requires #dg.dungeon > 1 — i.e. at least
           ;; TWO native Luanti dungeons in a single mapchunk — before
           ;; building a lair.  That's rare: most chunks have 0-1
           ;; dungeons, so lairs almost never trigger.  Lower to >= 1.
           (lambda _
             (substitute* "terrain.lua"
               (("#dg\\.dungeon > 1") "#dg.dungeon >= 1"))))
         (add-after 'lower-dungeon-count-threshold 'spawn-goblins-at-lair-gen
           ;; Mineclonia stubs out mcl_mobs.spawn_setup() in modern versions,
           ;; so the mod's natural spawning is a no-op — lair STRUCTURES
           ;; generate but stay empty.  Spawn goblins directly at lair build
           ;; time as a workaround, so players actually find inhabitants
           ;; when they discover a lair.
           ;; substitute* matches line-by-line, so anchor on the single
           ;; `        columns(cur_dg)` line (8-space indent inside the
           ;; goblairgen "LET'S DO IT!" body) and append the spawn block
           ;; right after it.
           (lambda _
             (substitute* "terrain.lua"
               (("columns\\(cur_dg\\)")
                "columns(cur_dg) ; do local _gob_types = {\"goblins:goblin_coal\",\"goblins:goblin_iron\",\"goblins:goblin_copper\",\"goblins:goblin_gold\",\"goblins:goblin_diamond\",\"goblins:goblin_digger\",\"goblins:goblin_hoarder\",\"goblins:goblin_snuffer\",\"goblins:goblin_fungiler\",\"goblins:goblin_cobble\"} local _gn = math.random(3, 6) for _ = 1, _gn do local _gp = vector.add(cur_dg, vector.new(math.random(-4,4), 2, math.random(-4,4))) local _gt = _gob_types[math.random(#_gob_types)] core.add_entity(_gp, _gt) core.log(\"action\", \"[goblins-lairgen] spawned \" .. _gt .. \" at \" .. vector.to_string(_gp)) end end"))))
         (add-after 'spawn-goblins-at-lair-gen 'nilsafe-time-of-day
           ;; Upstream goblins reads `self.time_of_day` in 3 places
           ;; (goblins.lua:44, behaviors.lua:83/84) and compares it to
           ;; numeric constants, but nothing ever assigns to it.  With
           ;; mcl_mobs.spawn_setup() being a no-op stub in modern
           ;; mineclonia, that field never gets populated, so on the
           ;; first do_custom tick the comparison `nil > 0.2` raises
           ;; "attempt to compare number with nil", killing the whole
           ;; server via on_step.  Wrap each read so it falls back to
           ;; the current global time-of-day if missing.
           (lambda _
             (substitute* (list "goblins.lua" "behaviors.lua")
               (("self\\.time_of_day") "(self.time_of_day or core.get_timeofday())"))))
         (add-after 'nilsafe-time-of-day 'register-modern-spawners
           ;; Upstream goblins registers natural spawning through
           ;; `mcl_mobs.spawn_setup({...})` in mc2_compat.lua.  In
           ;; mineclonia 0.120+ that function is a noop stub that just
           ;; emits a "will not spawn naturally" warning per goblin
           ;; type.  Result: lairs spawn goblins (via the entelequia
           ;; `spawn-goblins-at-lair-gen' patch above), but biome /
           ;; cave-based natural spawning is dead.
           ;;
           ;; Fix: add a side file that calls the modern
           ;; `mcl_mobs.register_spawner(...)' API for each goblin
           ;; type, modeled on mineclonia's own monster spawners
           ;; (mobs_mc.monster_spawner base).  Append `dofile' at the
           ;; end of init.lua so it runs after all goblin mob types
           ;; are registered.
           (lambda _
             (call-with-output-file "goblin_spawn_fix.lua"
               (lambda (port)
                 (display "-- entelequia patch: register modern mcl_mobs spawners for goblin
-- types.  Workaround for upstream goblins mod which still uses the
-- deprecated mcl_mobs.spawn_setup() API (a noop stub in mineclonia
-- 0.120+).  Modeled on mineclonia's mobs_mc:spider spawner.
--
-- Faithfully preserves the per-type constraints in upstream's
-- goblins_spawning_mc2.lua:
--   * max_light: matches upstream's max_light per goblin variant.
--   * max_height: matches upstream's depth ceiling per variant
--     (digger/cobble/snuffer/fungiler/coal at <= -16; copper -32;
--     iron -35; gold/hoarder at overworld_min+64; diamond at
--     overworld_min+32 — i.e. the deepest tier).
--   * weight: derived from upstream 'chance' (lower chance = more
--     common in old API; here translated to a relative weight, with
--     mobs_mc:spider weight=100 as the common-monster benchmark).
--
-- *Not preserved*: upstream restricts snuffer/fungiler/coal/iron/
-- copper/gold/diamond/hoarder to nodes adjacent to mcl_core:mossycobble
-- (or specific ore nodes).  Modern register_spawner has no cheap
-- way to express \"near node X\".  We compensate by giving those
-- types lower weights so they remain rare encounters outside lairs,
-- while still letting players find them in dim caves.

if not (mcl_mobs and mcl_mobs.register_spawner and mobs_mc and mobs_mc.monster_spawner) then
  core.log('warning', '[goblin_spawn_fix] required mcl_mobs/mobs_mc APIs missing; skipping')
  return
end

local OWMIN = (mcl_vars and mcl_vars.mg_overworld_min) or -128

local function gob_spawner(def)
  return table.merge(mobs_mc.monster_spawner, {
    name = def.name,
    spawn_category = 'monster',
    biomes = mobs_mc.monster_biomes,
    weight = def.weight,
    pack_min = 1,
    pack_max = def.pack_max or 1,
    max_light = def.max_light,
    min_height = OWMIN,
    max_height = def.max_height,
  })
end

-- Per-type config drawn from upstream's db_spawning table.
-- Old (chance, max_light, max_height) -> new (weight, max_light, max_height).
local SPAWNERS = {
  -- Common cave goblins: any stone, anywhere underground.
  { name = 'goblins:goblin_digger',    weight = 30, max_light = 12, max_height = -16, pack_max = 2 },
  { name = 'goblins:goblin_cobble',    weight = 30, max_light = 12, max_height = -16, pack_max = 2 },
  -- Mossy-cobble-tied (upstream); approximated as rare cave dwellers.
  { name = 'goblins:goblin_snuffer',   weight = 15, max_light = 14, max_height = -16, pack_max = 1 },
  { name = 'goblins:goblin_fungiler',  weight = 10, max_light = 10, max_height = -16, pack_max = 1 },
  -- Ore-tied tiers: progressively rarer + deeper.
  { name = 'goblins:goblin_coal',      weight = 15, max_light = 10, max_height = -16, pack_max = 2 },
  { name = 'goblins:goblin_copper',    weight = 15, max_light = 10, max_height = -32, pack_max = 2 },
  { name = 'goblins:goblin_iron',      weight = 15, max_light = 10, max_height = -35, pack_max = 2 },
  -- Deep-tier (bottom 32-64 blocks of overworld).
  { name = 'goblins:goblin_gold',      weight =  8, max_light = 10, max_height = OWMIN + 64, pack_max = 1 },
  { name = 'goblins:goblin_diamond',   weight =  8, max_light = 10, max_height = OWMIN + 32, pack_max = 1 },
  { name = 'goblins:goblin_hoarder',   weight =  5, max_light = 10, max_height = OWMIN + 64, pack_max = 1 },
}

local n = 0
for _, s in ipairs(SPAWNERS) do
  local ok, err = pcall(function()
    mcl_mobs.register_spawner(gob_spawner(s))
  end)
  if ok then
    n = n + 1
    core.log('action', string.format(
      '[goblin_spawn_fix] %s weight=%d max_light=%d max_height=%d',
      s.name, s.weight, s.max_light, s.max_height))
  else
    core.log('error', '[goblin_spawn_fix] ' .. s.name .. ': ' .. tostring(err))
  end
end
core.log('action', '[goblin_spawn_fix] registered ' .. n .. '/' .. #SPAWNERS .. ' goblin spawners')
" port)))
             (let ((out (open-file "init.lua" "a")))
               (display "\n-- entelequia patch: modern mcl_mobs spawner registration.\ndofile(path .. \"/goblin_spawn_fix.lua\")\n" out)
               (close-port out))))
         (add-after 'register-modern-spawners 'repopulate-existing-lairs
           ;; Retroactively populate lairs already in the world (chunks
           ;; generated before the lair-gen spawn patch).  Append an LBM
           ;; (Loading Block Modifier) that fires when chunks containing
           ;; goblins:cobble_with_moss / goblins:deepslate_with_moss load.
           ;; Idempotency: skip if a goblin already lives near the lair
           ;; (so chunks reloaded multiple times don't multiply goblins).
           ;; Rate limit: 1/24 random trigger keeps density sane (a lair
           ;; has ~30 moss nodes; expect ~1 spawn event per lair).
           (lambda _
             (let ((out (open-file "terrain.lua" "a")))
               (display "

-- entelequia patch: retroactively populate existing lairs on chunk load.
core.register_lbm({
  label = \"goblins: repopulate existing lairs\",
  name = \"goblins:repopulate_lair\",
  nodenames = {\"goblins:cobble_with_moss\", \"goblins:deepslate_with_moss\"},
  run_at_every_load = false,
  action = function(pos)
    if math.random(1, 24) ~= 1 then return end
    for _, obj in ipairs(core.get_objects_inside_radius(pos, 12)) do
      local ent = obj:get_luaentity()
      if ent and ent.name and ent.name:match(\"^goblins:goblin\") then
        return
      end
    end
    local types = {
      \"goblins:goblin_coal\",\"goblins:goblin_iron\",\"goblins:goblin_copper\",
      \"goblins:goblin_gold\",\"goblins:goblin_diamond\",\"goblins:goblin_digger\",
      \"goblins:goblin_hoarder\",\"goblins:goblin_snuffer\",\"goblins:goblin_fungiler\",
      \"goblins:goblin_cobble\"
    }
    local n = math.random(3, 6)
    for _ = 1, n do
      local p = vector.add(pos, vector.new(math.random(-4,4), 2, math.random(-4,4)))
      local t = types[math.random(#types)]
      core.add_entity(p, t)
      core.log(\"action\", \"[goblins-lbm] spawned \" .. t .. \" at \" .. vector.to_string(p))
    end
  end,
})

-- entelequia patch: auto-emerge a small spawn-area volume at server
-- start.  Luanti doesn't pre-generate chunks on fresh worlds until a
-- player arrives, so lair gen (and thus our spawn patch) never fires
-- before someone logs in.  Pre-emerging makes \"are goblins working?\"
-- verifiable from the server log.  Idempotent: on subsequent starts
-- the chunks already exist and emerge_area just loads them.
core.after(3, function()
  core.log(\"action\", \"[goblins-init] auto-emerging spawn area for lair generation\")
  core.emerge_area(vector.new(-256, -80, -256), vector.new(256, 16, 256), function(_, _, calls_remaining)
    if calls_remaining == 0 then
      core.log(\"action\", \"[goblins-init] spawn-area emerge complete\")
    end
  end)
end)
" out)
               (close-port out))))
         )))
    ;; luanti-mobs (Mobs Redo) is only needed for the build-time check phase
    ;; (which tests against Minetest Game).  At runtime with Mineclonia/VoxeLibre
    ;; the mod uses mcl_mobs directly — no luanti-mobs in the user profile.
    (inputs (list luanti-mobs))
    (home-page "https://codeberg.org/freelikegnu/goblins")
    (synopsis "Add goblins to Luanti")
    (description
     "(Respectfully) Destructive! Goblin NPCs burrow underground, build lairs, set traps and cultivate foodstuffs. They like to steal torches! ")
    ;; CC0: some textures and sounds
    (license (list license:cc0 license:expat))
    (properties `((upstream-name . "freelikegnu/mobs_goblins")))))

;;;
;;; Additional Luanti mob mods for the edison server.
;;;
;;; All of these were originally written for Minetest Game and depend on
;;; either TenPlus1's Mobs Redo (luanti-mobs) or ElCeejo's Creatura.  They
;;; load and run alongside Mineclonia's mcl_mobs ecosystem but their
;;; entities, drops and spawn rules are NOT integrated with mineclonia's
;;; biomes/items — expect a parallel mob ecosystem feel.
;;;

;; Newer Mobs Redo than upstream Guix (2021-12-12 has hard `default.*`
;; calls that crash in mineclonia).  The 2026-05-09 upstream guards
;; every `default.*` access behind `core.get_modpath("default")` and
;; adds explicit mcl_core compat.  Override transparently.
(define-public luanti-mobs
  (package
    (inherit (@ (gnu packages luanti) luanti-mobs))
    (name "luanti-mobs")
    (version "2026-05-09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/tenplus1/mobs_redo")
             (commit "03e6aada512e7faf2fb1cca5008dda0357baf97d")))
       (sha256
        (base32 "16fkp9j4j0vnljvl90y1y7qg5c4gd3jj6spi35ylb4l1lypdr8a2"))
       (file-name (git-file-name name version))))))

;; Same situation for mobs_monster: upstream Guix pins 2022-12-10.
;; The 2026-05-09 upstream has mineclonia compat and depends on the
;; matching newer mobs_redo.  Override to track our local luanti-mobs.
(define-public luanti-mobs-monster
  (package
    (inherit (@ (gnu packages luanti) luanti-mobs-monster))
    (name "luanti-mobs-monster")
    (version "2026-05-09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/tenplus1/mobs_monster")
             (commit "a21a1f71c762e97ff3f1c337c679a0b310e8a761")))
       (sha256
        (base32 "1mx4k8hxlvy18hclhvq7b0ywskw7bkjndc5qba82h5na0642wqdk"))
       (file-name (git-file-name name version))))
    (propagated-inputs (list luanti-mobs))))

(define-public luanti-mobs-skeletons
  (package
    (name "luanti-mobs-skeletons")
    (version "2026-04-21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/tenplus1/mobs_skeletons")
             (commit "4bee07f07b3578874844b6e21e48e778c7f74f7f")))
       (sha256
        (base32 "0yq66nikalnl6gfyhk118ggabirc4lrdjgac3rmh8f76sm3qn9ff"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'drop-default-dep
           ;; mod.conf has `depends = default, mobs`.  We don't ship Minetest
           ;; Game's `default` mod (mineclonia replaces it with mcl_core).
           ;; Make `default` optional so the mod loads in mineclonia; the
           ;; only drop reference is a steel-sword that just silently
           ;; no-ops without it.
           (lambda _
             (substitute* "mod.conf"
               (("^depends = default, mobs")  "depends = mobs")
               (("^optional_depends = ")      "optional_depends = default, "))))
         (add-after 'drop-default-dep 'replace-default-light-max
           ;; init.lua references `default.LIGHT_MAX` at top level (eager
           ;; eval).  In Luanti core LIGHT_MAX is always 14, so substitute.
           (lambda _
             (substitute* "init.lua"
               (("default\\.LIGHT_MAX") "14")))))))
    (inputs (list luanti-mobs))
    (home-page "https://codeberg.org/tenplus1/mobs_skeletons")
    (synopsis "Add skeleton mobs to Luanti")
    (description
     "Adds skeletons (with various weapons) to Luanti via the Mobs Redo API.")
    (license license:expat)
    (properties `((upstream-name . "TenPlus1/mobs_skeletons")))))

(define-public luanti-animalworld
  (package
    (name "luanti-animalworld")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Skandarella/Animal-World")
             (commit "ac835da96681774679ace90656812aab67e25b5c")))
       (sha256
        (base32 "17dfchhcvmaynddly7q7j8adf6fpvipcxr7ac50zpd2qclq6m61w"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'guard-default-sound-call
           ;; concretecrafting.lua eagerly calls default.node_sound_stone_defaults()
           ;; at top level.  Wrap in a guarded expression so it's nil-safe.
           (lambda _
             (substitute* "concretecrafting.lua"
               (("default\\.node_sound_stone_defaults\\(\\)")
                "((rawget(_G,\"default\") or {}).node_sound_stone_defaults or function() return {} end)()")))))))
    (inputs (list luanti-mobs))
    (home-page "https://github.com/Skandarella/Animal-World")
    (synopsis "Wilhelmine's Animal World — wildlife mob pack")
    (description
     "Adds many animal mobs (territorial, attacking, with multiple animations)
to Luanti.  Supports Minetest Game and Mineclonia/VoxeLibre via the Mobs
Redo API.  Includes hunting trophies (decorative animal heads).")
    (license license:expat)
    (properties `((upstream-name . "Liil/animalworld")))))

(define-public luanti-creatura
  (package
    (name "luanti-creatura")
    (version "2025-12-25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ElCeejo/Creatura")
             (commit "4eb507cf2433f0787691f560842deea79a1666f4")))
       (sha256
        (base32 "1b0s3d2pdmgmsyx6hn7lxzlqx7b1nb83ml3h0za379xfjryxa695"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (home-page "https://github.com/ElCeejo/Creatura")
    (synopsis "Performant, semi-modular mob API for Luanti")
    (description
     "Creatura is an alternative mob API for Luanti, focused on performance
and semi-modular mob construction.  It is the mob framework used by
@code{luanti-draconis} and other ElCeejo mods.")
    (license license:expat)
    (properties `((upstream-name . "ElCeejo/creatura")))))

(define-public luanti-draconis
  (package
    (name "luanti-draconis")
    (version "2026-02-18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ElCeejo/draconis")
             (commit "5ad66e400ec31aa8c6ca33e8a8c2510b6fa5d6fd")))
       (sha256
        (base32 "1gr15w29ykfil852p6h57h4simiwv01jbdqrjqrf28kyn6kqk5yh"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'guard-default-access
           ;; init.lua does `if default.node_sound_X then` which still
           ;; throws because `default` itself is nil.  Replace bare
           ;; `default.node_sound_` with a nil-safe lookup.
           (lambda _
             (substitute* "init.lua"
               (("default\\.node_sound_")
                "(rawget(_G,\"default\") or {}).node_sound_"))))
         (add-after 'guard-default-access 'guard-steel-ingot-lookup
           ;; nodes.lua:256 indexes minetest.registered_items[steel_ingot]
           ;; at load time, before the runtime mods_loaded callback can
           ;; rebind steel_ingot to mineclonia's mcl_core:iron_ingot.
           ;; default:steel_ingot doesn't exist → nil deref crashes.
           (lambda _
             (substitute* "nodes.lua"
               (("minetest\\.registered_items\\[steel_ingot\\]\\.stack_max")
                "(minetest.registered_items[steel_ingot] or {}).stack_max"))))
         (add-after 'guard-steel-ingot-lookup 'fix-is-creative-enabled-arg
           ;; Modern Mineclonia's mcl_gamemode.is_creative_enabled requires
           ;; a STRING playername, but Draconis passes the player OBJECT
           ;; in 4 places (behaviors.lua:144, api.lua:1352/1532,
           ;; craftitems.lua:115).  Dragon AI's find_target hits
           ;; behaviors.lua:144 on every tick once a player is in range —
           ;; the resulting "requires a string" error propagates through
           ;; creatura's on_step and crashes the whole server.  Rewrite
           ;; each call to pass a name string, guarded so it falls back
           ;; to "" if the variable is nil.
           (lambda _
             (substitute* (list "api/api.lua"
                                "api/behaviors.lua"
                                "craftitems.lua")
               (("minetest\\.is_creative_enabled\\(player\\)")
                "minetest.is_creative_enabled(player and player.get_player_name and player:get_player_name() or \"\")")
               (("minetest\\.is_creative_enabled\\(target\\)")
                "minetest.is_creative_enabled(target and target.get_player_name and target:get_player_name() or \"\")")
               (("minetest\\.is_creative_enabled\\(clicker\\)")
                "minetest.is_creative_enabled(clicker and clicker.get_player_name and clicker:get_player_name() or \"\")")))))))
    (inputs (list luanti-creatura))
    (home-page "https://github.com/ElCeejo/draconis")
    (synopsis "Adds advanced Dragons and powerful equipment to Luanti")
    (description
     "Draconis adds Fire and Ice Dragons to Luanti, with full lifecycle
(eggs, hatching, growth), flight, breath weapons, dragon-themed armor and
weapons.  Built on the Creatura mob API.")
    (license license:expat)
    (properties `((upstream-name . "ElCeejo/draconis")))))

(define-public luanti-far-spawn
  ;; Tiny in-tree mod that scatters new players around the world on a
  ;; circle of slots, so families don't all land at (0, ?, 0) together.
  ;; Source lives at entelequia/packages/luanti-far-spawn/.
  (package
    (name "luanti-far-spawn")
    (version "0.3.0")
    (source (local-file "luanti-far-spawn" #:recursive? #t))
    (build-system luanti-mod-build-system)
    (home-page "https://github.com/rafaelpalomar/dotfiles")
    (synopsis "Scatter new players across a far-away spawn circle")
    (description
     "Hooks @code{core.register_on_newplayer} and teleports each
brand-new player to one of @math{N=8} slots on a circle of radius
@math{2000} around the world origin.  Slot assignment is round-robin
and persistent across server restarts via the mod's mod_storage.  Also
sets the player's respawn point so death sends them back to their
assigned slot rather than world origin.")
    (license license:expat)))

(define-public luanti-forgotten-monsters
  (package
    (name "luanti-forgotten-monsters")
    (version "0.60.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/pixelzone/forgotten_monsters_reworked")
             (commit "01010b7cc8f4db3bb80a110795d25ad7ce0d82d0")))
       (sha256
        (base32 "0v63l5631dmb6jpjb173c6da053kbvsb4nz07b56vl6kvjaxlf46"))
       (file-name (git-file-name name version))))
    (build-system luanti-mod-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-foreign-namespace-registrations
           ;; Upstream registers a few entities under foreign mod prefixes
           ;; (e.g. `rb_animals:skeleton_swordfish` in a file shipped by
           ;; `forgotten_monsters`).  Luanti rejects that since 5.x;
           ;; rewrite the mob names to use the `forgotten_monsters:` prefix
           ;; so the mod loads cleanly.
           (lambda _
             (substitute* (find-files "fg_monsters" "\\.lua$")
               (("\"rb_animals:") "\"forgotten_monsters:")))))))
    (inputs (list luanti-mobs))
    (home-page "https://codeberg.org/pixelzone/forgotten_monsters_reworked")
    (synopsis "Forgotten Monsters (Reworked) — bosses and monsters for Luanti")
    (description
     "Adds skulls, spectrums and bosses to Luanti gameplay, inspired by the
\"Forbidden Island\" mod.  Uses the Mobs Redo API.")
    (license (list license:expat license:cc-by-sa4.0))
    (properties `((upstream-name . "pixelzone/forgotten_monsters")))))


;;;
;;; netheroes2 — online-multiplayer fork of fheroes2 (Heroes of Might & Magic II)
;;;
;;; The stock fheroes2 engine has only hot-seat multiplayer; this Bitbucket
;;; fork (heroes2.online) adds network play against the public server at
;;; https://www.heroes2.online/game1 (hard-coded in game_network.cpp, reached
;;; via libcurl).  Same asset requirement as fheroes2: the original HoMM II
;;; data in ~/.local/share/fheroes2.  The fork's CMake still builds a binary
;;; named `fheroes2' and installs a fheroes2.desktop/icon, which would collide
;;; with the fheroes2 package in a shared profile, so we rename the binary to
;;; `netheroes2' and drop the fork's desktop entry (we ship our own).
;;;
(define-public netheroes2
  (package
    (name "netheroes2")
    (version "0.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/fheroes2/netheroes2.git")
             (commit "1f32fe89c9eadf300988500c897afbf009692e0c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n9yaivhh6aq0rh2pvvyv4zgb3rnn40x5qm4gm9y3yfhhyj9bsa9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          ;; Default the in-game server URL to our self-hosted heroes-server on
          ;; edison (LAN), instead of the public heroes2.online.  Users can still
          ;; override it in netheroes2's network menu.
          (add-after 'unpack 'patch-default-server-url
            (lambda _
              (substitute* "src/fheroes2/game/game_network.cpp"
                (("https://www\\.heroes2\\.online/game1")
                 "http://192.168.88.14:3030"))))
          (add-after 'install 'rename-to-netheroes2
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (f2  (string-append bin "/fheroes2")))
                (when (file-exists? f2)
                  (rename-file f2 (string-append bin "/netheroes2"))))
              ;; Drop the fork's fheroes2.desktop so it can't collide with the
              ;; fheroes2 package; the curie home profile ships its own entry.
              (let ((apps (string-append #$output "/share/applications")))
                (when (file-exists? apps)
                  (for-each delete-file (find-files apps "\\.desktop$")))))))))
    (native-inputs (list gettext-minimal))
    (inputs
     (list curl
           (sdl-union (list sdl2 sdl2-mixer sdl2-net))
           zlib))
    (home-page "https://heroes2.online/")
    (synopsis "Online-multiplayer fork of the fheroes2 HoMM II engine")
    (description
     "netheroes2 is a fork of @code{fheroes2} that adds network multiplayer via
the public heroes2.online server.  Like fheroes2, it needs the original Heroes
of Might and Magic II game assets in @file{~/.local/share/fheroes2}.")
    (license license:gpl2)))
