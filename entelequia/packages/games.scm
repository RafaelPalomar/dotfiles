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
  #:use-module (gnu packages gl)         ; mesa
  #:use-module (gnu packages luanti)         ; mesa
  #:use-module (gnu packages audio)      ; openal, pulseaudio, alsa-lib
  #:use-module (gnu packages xiph)       ; libogg
  #:use-module (gnu packages gcc)        ; gcc "lib"
  #:use-module (gnu packages sdl)        ; sdl2
  #:use-module (gnu packages gtk)        ; gtk+ (GTK3), gtk+-2 (GTK2)
  #:use-module (gnu packages glib)       ; glib
  #:use-module (gnu packages pulseaudio) ; pulseaudio (libpulse-simple)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system luanti)
  #:export (make-game-launcher
            make-game-fhs-launcher
            make-scummvm-launcher
            make-wine-game-launcher
            make-proton-game-launcher
            proton-ge-10-34
            ;; GOG games
            gog-crypt-of-the-necrodancer
            gog-terraria
            gog-wizard-of-legend
            gog-slay-the-spire
            gog-death-road-to-canada
            gog-torchlight-2
            gog-duskers
            gog-papers-please
            gog-gobliiins
            gog-gobliins-2
            gog-goblins-quest-3
            gog-they-are-billions
            gog-9-kings
            gog-he-is-coming
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
              "(use-modules (guix channels))
(list (channel
        (name 'guix)
        (url \"https://git.guix.gnu.org/guix.git\")
        (branch \"master\")
        (commit \"6a483ed7c607b01003edb9cb118c9f89c9d457e9\")
        (introduction
          (make-channel-introduction
            \"9edb3f66fd807b096b48283debdcddccfea34bad\"
            (openpgp-fingerprint
              \"BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA\"))))
      (channel
        (name 'nonguix)
        (url \"https://gitlab.com/nonguix/nonguix\")
        (branch \"master\")
        (commit \"f5338f63fce69622ce06f93fe02524967e1f30d4\")
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
                                       (extra-args '()))
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
               ,@(map (lambda (pair)
                        `(format port "export ~a=\"~a\"~%"
                                 ,(car pair) ,(cdr pair)))
                      extra-env)
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
               (format port "      export WINEFSYNC=0;\\\n")
               (format port "      export WINEESYNC=0;\\\n")
               ;; Disable Xalia (Proton's accessibility/SDL helper) — its
               ;; SDL_VideoInit fails inside the Guix FHS container with
               ;; "Video driver  not supported" and aborts the launch.
               ;; The game itself doesn't need Xalia; it only matters for
               ;; Steam Deck on-screen-keyboard hints.
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
                "goblins.compat_mode == \"mc2\" and goblins_lair_rail_corridor_chance ~= 0 and goblins_lair_chance ~= 0 and mcl_structures.registered_structures[\"mineshaft\"]")))))))
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
