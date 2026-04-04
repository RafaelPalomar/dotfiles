(define-module (entelequia system lib edison-services)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services audio)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages linux)
  #:use-module (entelequia system lib server-services)
  #:use-module (guix gexp)
  #:use-module (sops packages sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:export (edison-data-dir-service
            edison-mpd-service
            edison-nvidia-devices-service
            edison-nfs-media-service
            edison-sops-service
            edison-arm-udev-service
            edison-arm-config-patch-service
            edison-container-services))

;;; Edison multimedia server services
;;;
;;; Containers use rootless Podman (user "rafael") with Tailscale sidecars
;;; for remote access. NVIDIA GPUs are passed via --device for Jellyfin
;;; hardware transcoding (NVENC). Rafael must be in the "video" group so
;;; rootless Podman can access /dev/nvidia* (owned root:video mode 0660).
;;;
;;; Media is read from /media (NFS-mounted from lovelace:/data/media).
;;; Container data lives on /data (local XFS disk, /dev/sdb1).

;;; SOPS secrets file (must be created before deploying — see below)
;;;
;;; Pre-deployment setup:
;;;   1. On Edison: gpg --homedir /var/lib/sops --full-generate-key
;;;   2. Export: gpg --homedir /var/lib/sops --armor --export > /tmp/edison-sops.asc
;;;   3. On workstation: gpg --import /tmp/edison-sops.asc
;;;   4. Create sops/edison.yaml: sops --encrypt --pgp <fingerprint> /dev/stdin < secrets.yaml
;;;
;;; Required keys in sops/edison.yaml:
;;;   tailscale.jellyfin_authkey
;;;   tailscale.navidrome_authkey
;;;   tailscale.arm_authkey

(define %sops-edison
  (local-file "../../../sops/edison.yaml"))

;;;
;;; /data directory structure — created at activation time
;;;

;;; edison-data-dir-service: creates all required /data subdirectories at boot.
;;; Must run after file-system-/data is mounted. Idempotent (mkdir -p).
;;; Note: /data/mpd is owned by the mpd system user (not rafael) because
;;; mpd-service-type runs MPD as the 'mpd' user.
(define edison-data-dir-service
  (list
   (simple-service 'edison-data-dirs
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       ;; Dirs owned by rafael (containers run as rafael)
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let* ((pw  (getpwnam "rafael"))
                                 (uid (passwd:uid pw))
                                 (gid (passwd:gid pw)))
                            (chown dir uid gid)))
                        '("/data/tailscale/jellyfin"
                          "/data/tailscale/navidrome"
                          "/data/tailscale/arm"
                          "/data/jellyfin"
                          "/data/jellyfin/config"
                          "/data/jellyfin/cache"
                          "/data/navidrome"
                          "/data/caddy"))
                       ;; /run/user/1001: rootless Podman requires XDG_RUNTIME_DIR to exist.
                       ;; Rafael never logs in interactively, so elogind never creates it.
                       ;; We create it here so it survives service restarts.
                       (let* ((pw  (getpwnam "rafael"))
                              (uid (passwd:uid pw))
                              (gid (passwd:gid pw)))
                         (mkdir-p "/run/user/1001")
                         (chown "/run/user/1001" uid gid)
                         (chmod "/run/user/1001" #o700))
                       ;; Enable elogind user lingering for rafael.
                       ;; Without this, pam_elogind.so (present in the 'su' PAM stack) destroys
                       ;; /run/user/1001 when any 'su rafael' session ends (e.g. udev arm-trigger).
                       ;; This deletes pasta sockets and crun state, bringing down all container
                       ;; networking.  Lingering tells elogind to keep /run/user/1001 alive
                       ;; permanently, just as it would for an interactively logged-in user.
                       ;; The linger file at /var/lib/elogind/linger/rafael persists across reboots.
                       (let ((linger-dir "/var/lib/elogind/linger"))
                         (mkdir-p linger-dir)
                         (let ((linger-file (string-append linger-dir "/rafael")))
                           (unless (file-exists? linger-file)
                             (call-with-output-file linger-file (lambda (p) #t)))))
                       ;; /data/arm and subdirs: owned by container uid 1000 of the ARM container.
                       ;; In rootless Podman (rafael uid 1001, subuid starts at 231072):
                       ;;   container uid 0  = host uid 1001 (rafael)
                       ;;   container uid N  = host uid 231072 + N - 1  (for N >= 1)
                       ;; ARM runs its 'arm' user as uid 1000 inside the container,
                       ;; which maps to host uid 232071 (231072 + 1000 - 1).
                       (let ((arm-uid 232071)
                             (arm-gid 232071))
                         (for-each
                          (lambda (dir)
                            (mkdir-p dir)
                            (chown dir arm-uid arm-gid)
                            (chmod dir #o755))
                          '("/data/arm"
                            "/data/arm/logs"
                            "/data/arm/logs/progress"))
                         ;; arm.log is created by ARM on first run; ensure it is
                         ;; owned by the arm user so subsequent runs can append to it.
                         (let ((arm-log "/data/arm/logs/arm.log"))
                           (unless (file-exists? arm-log)
                             (call-with-output-file arm-log (lambda (p) #f)))
                           (chown arm-log arm-uid arm-gid))
                         ;; /media/rips subdirs (NFS mount from Lovelace): ensure the arm
                         ;; user owns the working directories so cdparanoia and abcde can
                         ;; write there.  The NFS root (/media/rips) is sticky+world-writable
                         ;; so we can create/chown inside it even before the arm container runs.
                         (for-each
                          (lambda (dir)
                            (when (file-exists? "/media/rips")
                              (mkdir-p dir)
                              (chown dir arm-uid arm-gid)
                              (chmod dir #o755)))
                          '("/media/rips/raw"
                            "/media/rips/transcode"
                            "/media/rips/completed"))
                         ;; Jellyfin media roots: world-writable so ARM and manual
                         ;; additions can both deposit files here.
                         (for-each
                          (lambda (dir)
                            (when (file-exists? "/media")
                              (mkdir-p dir)
                              (chmod dir #o1777)))
                          '("/media/movies"
                            "/media/tv"))
                         ;; arm.yaml seed: write a minimal file if absent so the
                         ;; arm-config-patch shepherd service can patch the TMDB key
                         ;; without waiting for ARM to generate the file first.
                         ;; ARM's config.py merges this with its default template on
                         ;; first start (user values win) and expands it to full config.
                         ;; Always write abcde.conf so deploys keep it in sync with this config.
                         ;; OUTPUTDIR uses the container path /home/arm/Music (capital M),
                         ;; which is mounted from /media/music on the host via NFS.
                         (let ((abcde-conf "/data/arm/abcde.conf"))
                           (call-with-output-file abcde-conf
                             (lambda (p)
                               (display
                                "CDDBMETHOD=musicbrainz\n\
OUTPUTTYPE=flac\n\
FLACOPTS='-s -8 --replay-gain'\n\
OUTPUTDIR=/home/arm/Music\n\
OUTPUTFORMAT='${ARTISTFILE}/${ALBUMFILE}/${TRACKNUM}. ${TRACKFILE}'\n\
VAOUTPUTFORMAT='Various Artists/${ALBUMFILE}/${TRACKNUM}. ${ARTISTFILE} - ${TRACKFILE}'\n\
ACTIONS=cddb,read,getalbumart,encode,embedalbumart,tag,move,clean\n\
EMBEDALBUMART=y\n\
PADTRACKS=y\n\
CDROMREADERSYNTAX=cdparanoia\n\
CDPARANOIAOPTS=--never-skip=40\n\
mungefilename () {\n\
  echo \"$@\" | sed -e 's/[^-[:alnum:] _.,()!]//g' | sed -e 's/  */ /g' | sed -e 's/^ //;s/ $//'\n\
}\n\
# Override metaflac to inject MIME type when embedding cover art.\n\
# abcde sources this file so this function shadows the real binary.\n\
# metaflac 1.3.x cannot auto-detect MIME type when the Cover Art Archive\n\
# returns PNG (or WebP) with a .jpg filename; this wrapper uses 'file' to\n\
# detect the actual type and passes it via the full picture spec.\n\
metaflac () {\n\
  local real; real=\\$(command -v metaflac)\n\
  local args=(); local i\n\
  for arg in \"\\$@\"; do\n\
    if [ \"\\${arg#--import-picture-from=}\" != \"\\$arg\" ]; then\n\
      local pic=\"\\${arg#--import-picture-from=}\"\n\
      if [ -f \"\\$pic\" ] && [ \"\\$(printf '%s' \"\\$pic\" | grep -c '|')\" -eq 0 ]; then\n\
        local mime; mime=\\$(file --mime-type -b \"\\$pic\" 2>/dev/null)\n\
        case \"\\$mime\" in\n\
          image/jpeg|image/png|image/gif)\n\
            arg=\"--import-picture-from=3|\\${mime}|||\\${pic}\" ;;\n\
          *) return 0 ;;\n\
        esac\n\
      fi\n\
    fi\n\
    args+=(\"\\$arg\")\n\
  done\n\
  \"\\$real\" \"\\${args[@]}\"\n\
}\n"
                                p)))
                           (chown abcde-conf arm-uid arm-gid))
                         ;; Write MakeMKV settings.conf with the license key decrypted by SOPS.
                         ;; The ARM container's arm user home (/home/arm) is not persisted, but
                         ;; ARM copies this file into the container from /etc/arm/config/.MakeMKV/.
                         ;; Key is stored in /run/secrets/makemkv_license_key by the sops service.
                         (let* ((makemkv-dir  "/data/arm/.MakeMKV")
                                (settings     (string-append makemkv-dir "/settings.conf"))
                                (key-file     "/run/secrets/makemkv_license_key"))
                           (mkdir-p makemkv-dir)
                           (chown makemkv-dir arm-uid arm-gid)
                           (chmod makemkv-dir #o755)
                           (when (file-exists? key-file)
                             (let ((key (string-trim-right
                                         (call-with-input-file key-file read-string))))
                               (call-with-output-file settings
                                 (lambda (p)
                                   (format p "app_Key = ~s\n" key)))
                               (chown settings arm-uid arm-gid)
                               (chmod settings #o600))))
                       ;; Write the ARM disc-trigger script called by the host udev rule.
                       ;; The ARM container can't receive kernel udev events (netlink is
                       ;; network-namespace scoped), so the host udev rule calls this script.
                       ;; Using /run (tmpfs) so it is always writable at activation time.
                       (let ((trigger "/run/arm-trigger.sh"))
                         (call-with-output-file trigger
                           (lambda (p)
                             (display
                              "#!/bin/sh\n\
# Trigger ARM rip inside the arm container when a disc is inserted.\n\
# Called by the 90-arm-disc-trigger.rules udev rule on host.\n\
# $1 = kernel device name (e.g. sr1)\n\
DEVNAME=\"$1\"\n\
LOG=/var/log/arm-trigger.log\n\
echo \"$(date): arm-trigger fired for $DEVNAME\" >> \"$LOG\"\n\
# cd away from /root so su rafael can chdir to rafael's home\n\
cd /\n\
# Pass a full PATH so podman is found under su's minimal environment.\n\
# XDG_RUNTIME_DIR points to rafael's (uid 1001) podman socket directory.\n\
PODMAN_PATH=\"PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin\"\n\
PODMAN_ENV=\"XDG_RUNTIME_DIR=/run/user/1001 $PODMAN_PATH\"\n\
# ARM's identify.py must mount the disc at /mnt/dev/$DEVNAME to inspect\n\
# the filesystem for BDMV/VIDEO_TS directories.  Mounting a block device\n\
# inside a rootless container is not possible (kernel restriction on\n\
# unprivileged user namespaces).  Instead, we use nsenter to enter the\n\
# container's mount namespace as host root (this script runs as root via\n\
# udev) and mount the disc there before ARM's identification step.\n\
ARM_PID=$(/run/current-system/profile/bin/env $PODMAN_ENV su -s /bin/sh rafael -c \\\n\
  \"podman inspect arm --format '{{.State.Pid}}'\" 2>/dev/null)\n\
if [ -n \"$ARM_PID\" ] && [ \"$ARM_PID\" != \"0\" ]; then\n\
  MOUNT_TARGET=\"/mnt/dev/$DEVNAME\"\n\
  # Only mount if not already mounted (idempotent)\n\
  if ! /run/current-system/profile/bin/nsenter -t \"$ARM_PID\" --mount -- /bin/mountpoint -q \"$MOUNT_TARGET\" 2>/dev/null; then\n\
    /run/current-system/profile/bin/nsenter -t \"$ARM_PID\" --mount -- /bin/mkdir -p \"$MOUNT_TARGET\"\n\
    /run/current-system/profile/bin/nsenter -t \"$ARM_PID\" --mount -- \\\n\
      /bin/mount -t udf,iso9660 -o ro \"/dev/$DEVNAME\" \"$MOUNT_TARGET\" >> \"$LOG\" 2>&1\n\
    echo \"$(date): pre-mounted /dev/$DEVNAME at $MOUNT_TARGET (exit $?)\" >> \"$LOG\"\n\
  fi\n\
  # Ensure arm user can open /dev/$DEVNAME for tray_status() ioctl.\n\
  # In rootless podman the container devtmpfs creates the node as root:root 0700;\n\
  # nsenter as host root can relax permissions so arm (uid 1000) can open it.\n\
  /run/current-system/profile/bin/nsenter -t \"$ARM_PID\" --mount -- \\\n\
    /bin/chmod a+rw \"/dev/$DEVNAME\" >> \"$LOG\" 2>&1\n\
  echo \"$(date): chmod /dev/$DEVNAME (exit $?)\" >> \"$LOG\"\n\
else\n\
  echo \"$(date): arm container not running, skipping pre-mount\" >> \"$LOG\"\n\
fi\n\
/run/current-system/profile/bin/env $PODMAN_ENV \\\n\
  su -s /bin/sh rafael -c \\\n\
  \"podman exec --user arm arm \\\n\
   python3 /opt/arm/arm/ripper/main.py -d $DEVNAME\" \\\n\
  >> \"$LOG\" 2>&1\n\
echo \"$(date): arm-trigger exit $? for $DEVNAME\" >> \"$LOG\"\n"
                              p)))
                         (chmod trigger #o755))
                       ;; Write a minimal arm.yaml if absent so the shepherd service can
                       ;; patch the TMDB key without polling for ARM to generate the file.
                       ;; INSTALLPATH is required by config.py to find the default template.
                       ;; ARM merges this with /opt/arm/setup/arm.yaml on first start and
                       ;; expands it to the full config (user values win via update()).
                       (let ((arm-yaml "/data/arm/arm.yaml"))
                         (unless (file-exists? arm-yaml)
                           (call-with-output-file arm-yaml
                             (lambda (p)
                               (display "INSTALLPATH: \"/opt/arm\"\n\
LOGPATH: \"/home/arm/logs/\"\n\
DBFILE: \"/home/arm/db/arm.db\"\n\
METADATA_PROVIDER: \"omdb\"\n\
TMDB_API_KEY: \"\"\n" p)))
                           (chown arm-yaml arm-uid arm-gid)
                           (chmod arm-yaml #o644))
                       ;; Deploy patched ARM Python files from the dotfiles source tree.
                       ;; ARM's arm_user_files_setup.sh does "chown -R arm:arm /opt/arm",
                       ;; which fails if bind-mounted files are owned by a different uid.
                       ;; Copying here (rather than bind-mount of source) lets ARM chown
                       ;; the file inside the container without affecting the source tree.
                       ;;
                       ;; arm-identify.py: fixes find_mount to fall back to /mnt/dev/<dev>
                       ;;   when findmnt returns the device path itself (devtmpfs stub).
                       ;; arm-system-drives.py: fixes CDROM_DRIVE_STATUS ioctl failure on
                       ;;   rootless podman sr* stubs; returns DISC_OK so ARM can proceed.
                       (let ((identify-src    #$(local-file "arm-identify.py"))
                             (sysdrv-src      #$(local-file "arm-system-drives.py"))
                             (musicbrainz-src #$(local-file "arm-music-brainz.py"))
                             (postproc-src    #$(local-file "arm-post-process.sh")))
                         (for-each
                          (lambda (src dst mode)
                            (copy-file src dst)
                            (chown dst arm-uid arm-gid)
                            (chmod dst mode))
                          (list identify-src sysdrv-src musicbrainz-src postproc-src)
                          (list "/data/arm/identify.py"
                                "/data/arm/system_drives.py"
                                "/data/arm/music_brainz.py"
                                "/data/arm/post-process.sh")
                          (list #o644 #o644 #o644 #o755)))))
                       ;; Dirs owned by mpd (mpd-service-type runs as 'mpd' user)
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let* ((pw  (getpwnam "mpd"))
                                 (uid (passwd:uid pw))
                                 (gid (passwd:gid pw)))
                            (chown dir uid gid)))
                        '("/data/mpd"
                          "/data/mpd/playlists"))))))

;;;
;;; NFS /media mount readiness service
;;;
;;; file-system-/media succeeds even when the NFS mount fails at boot
;;; (mount-may-fail? #t). This service waits until /media is actually
;;; mounted, retrying the mount if necessary. All containers that read from
;;; /media depend on nfs-media instead of file-system-/media so they are
;;; guaranteed to start only after the NFS share is accessible.

(define %nfs-media-wait-script
  (program-file "nfs-media-wait"
    #~(begin
        (let loop ((n 12))         ; 12 × 5s = 60s max
          (cond
            ((zero? n)
             (format (current-error-port)
                     "nfs-media-wait: /media not available after 60s~%")
             (primitive-exit 1))
            ((catch #t
               (lambda () (stat "/media/rips") #t)
               (lambda _ #f))
             (format #t "nfs-media-wait: /media is mounted~%")
             (primitive-exit 0))
            (else
             (format #t "nfs-media-wait: /media not mounted, retrying (~a/12)...~%"
                     (- 13 n))
             (system* #$(file-append util-linux "/bin/mount") "-t" "nfs"
                      "-o" "noatime,rsize=131072,wsize=131072,vers=4,soft,intr,timeo=150,retrans=3"
                      "192.168.88.46:/data/media" "/media")
             (sleep 5)
             (loop (- n 1))))))))  ; closes cond, loop, let, begin, #~, program-file, define

(define edison-nfs-media-service
  (list
   (simple-service 'nfs-media-wait
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (provision '(nfs-media))
                     (requirement '(file-system-/media networking))
                     (one-shot? #t)
                     (documentation
                      "Wait for /media NFS mount; retry mount if needed.")
                     (start #~(make-forkexec-constructor
                               (list #$%nfs-media-wait-script)
                               #:log-file "/var/log/nfs-media.log"))
                     (stop #~(make-kill-destructor)))))))

;;;
;;; MPD — Music Player Daemon
;;;
;;; Reads music from /media/music (NFS). Exposes:
;;;   - MPD protocol on port 6600 (mpc, Android clients like M.A.L.P.)
;;;   - HTTP audio stream on port 8000 (web clients, Navidrome)
;;;
;;; Note: mpd system user must be able to read /media/music. The NFS
;;; export uses no_root_squash; ensure media files are world-readable
;;; (chmod -R a+rX /data/media/music on lovelace) or run MPD as rafael.
(define edison-mpd-service
  (list
   (service mpd-service-type
            (mpd-configuration
             (music-directory "/media/music")
             (playlist-directory "/data/mpd/playlists")
             (db-file "/data/mpd/database")
             (state-file "/data/mpd/state")
             (default-port 6600)
             ;; Wait for NFS mount to be accessible before starting
             (shepherd-requirement '(nfs-media))
             ;; No PulseAudio on a headless server
             (environment-variables '())
             (outputs
              (list
               ;; HTTP stream — Navidrome and web clients read from here
               (mpd-output
                (name "HTTP Stream")
                (type "httpd")
                (always-on? #t)
                (extra-options '(("encoder" . "vorbis")
                                 ("port"    . "8000"))))
               ;; Null output — allows MPD to run without audio hardware
               (mpd-output
                (name "Null")
                (type "null"))))))))

;;;
;;;
;;;
;;; NVIDIA device nodes — work around udev race condition
;;;
;;; The `90-nvidia.rules` udev rule has a TEST!="/dev/nvidia-uvm" condition that
;;; prevents nvidia-modprobe from running when the nvidia-uvm device already exists.
;;; Since the nvidia-uvm module loads before the PCI bind event fires for the main
;;; NVIDIA driver, the condition always fails → /dev/nvidia0, /dev/nvidiactl are
;;; never created at boot.
;;;
;;; This one-shot service runs `udevadm trigger` after udev is up to force device
;;; node creation. Jellyfin depends on it.
;;;

;;; nvidia-modprobe -c0 / -c1 creates /dev/nvidia0 and /dev/nvidia1.
;;; /dev/nvidiactl (major 195, minor 255) is not created by nvidia-modprobe;
;;; it must be mknod'd manually.  The setuid nvidia-modprobe is installed at
;;; /run/privileged/bin/nvidia-modprobe by nonguix-transformation-nvidia.

(define %nvidia-device-setup
  (program-file "nvidia-device-setup"
    #~(begin
        (system* "/run/privileged/bin/nvidia-modprobe" "-c0")
        (system* "/run/privileged/bin/nvidia-modprobe" "-c1")
        (unless (file-exists? "/dev/nvidiactl")
          (system* #$(file-append coreutils "/bin/mknod")
                   "-m" "660" "/dev/nvidiactl" "c" "195" "255")
          (system* #$(file-append coreutils "/bin/chgrp") "video" "/dev/nvidiactl"))
        ;; Run nvidia-smi to initialise both GPUs.  This triggers the kernel
        ;; to create /dev/nvidia-caps/nvidia-cap1 and nvidia-cap2, which are
        ;; created lazily on first NVENC/NVDEC access and therefore do not
        ;; exist until someone queries the driver.  Podman checks that
        ;; --device paths exist before starting a container, so the caps
        ;; devices must be present before the ARM/Jellyfin containers start.
        (system* "/run/current-system/profile/bin/nvidia-smi"
                 "--query-gpu=name" "--format=csv,noheader")
        ;; nvidia-cap1 (minor 1) = NVENC capability device.
        ;; The kernel creates it mode 0400 (root-only).  Rootless Podman containers
        ;; running as non-root cannot access it, so NVENC fails with
        ;; "Cannot load libnvidia-encode.so.1".  Make it world-readable so that
        ;; containers with --device=/dev/nvidia-caps/nvidia-cap1 can use NVENC.
        ;; nvidia-cap2 (minor 2) is already 0444; no change needed.
        (when (file-exists? "/dev/nvidia-caps/nvidia-cap1")
          (system* #$(file-append coreutils "/bin/chmod")
                   "a+r" "/dev/nvidia-caps/nvidia-cap1"))
        ;; Build /var/lib/nvidia-container-libs containing only NVIDIA driver
        ;; .so files with symlinks fully resolved (cp -L = dereference).
        ;;
        ;; The Guix nvidia profile libs are all symlinks that ultimately point
        ;; into /gnu/store.  Mounting the profile lib dir with LD_LIBRARY_PATH
        ;; causes Debian containers to pick up Guix-compiled Mesa/glibc-2.38
        ;; libs alongside the nvidia ones, crashing HandBrakeCLI/ffmpeg on
        ;; start.  This directory holds only libnvidia-*/libcuda*/libnvcuvid*
        ;; as real .so files (no /gnu/store dependency, no glibc conflict).
        ;; Recreated fresh at each boot so it stays in sync after driver updates.
        (let* ((sh     #$(file-append bash-minimal "/bin/sh"))
               (cp     #$(file-append coreutils "/bin/cp"))
               (rm     #$(file-append coreutils "/bin/rm"))
               (mkdir  #$(file-append coreutils "/bin/mkdir"))
               (target "/var/lib/nvidia-container-libs"))
          (system* mkdir "-p" target)
          ;; Wipe stale files so old driver versions don't linger after updates
          (system* sh "-c" (string-append rm " -f " target "/*.so*"))
          (for-each
           (lambda (glob)
             (system* sh "-c"
                      (string-append
                       "for f in /run/current-system/profile/lib/" glob "; do "
                       "[ -e \"$f\" ] && " cp " -Lf \"$f\" " target "/; "
                       "done")))
           ;; Globs covering all NVENC/NVDEC/CUDA driver libs.
           ;; cp -L dereferences each symlink so the target dir contains
           ;; actual ELF binaries readable by the Debian container image.
           '("libnvidia-*.so*"
             "libcuda.so*"
             "libnvcuvid.so*"
             "libnvrtc*.so*"
             "libnvoptix*.so*"))))))

(define edison-nvidia-devices-service
  (list
   (simple-service 'nvidia-devices
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (provision '(nvidia-devices))
                     (requirement '(udev))
                     (one-shot? #t)
                     (start #~(make-forkexec-constructor
                               (list #$%nvidia-device-setup)
                               #:log-file "/var/log/nvidia-devices.log"))
                     (stop #~(make-kill-destructor))
                     (documentation "Create NVIDIA device nodes at boot."))))))

;;;
;;; ARM config patch service — runs after sops-secrets, patches arm.yaml
;;;
;;; arm.yaml lives at /data/arm/arm.yaml (host) = /etc/arm/config/arm.yaml
;;; (container).  The activation service writes a minimal seed file if absent,
;;; so the file is guaranteed to exist before ARM starts.  ARM's config.py reads
;;; it at Flask startup, merges with the default template (user values win via
;;; arm_config.update(cur_cfg)), and rewrites the full config.  Our patch runs
;;; after sops-secrets and sets TMDB_API_KEY; ARM's per-rip processes (podman
;;; exec python3 main.py) each re-import config fresh, so no container restart
;;; is needed — the next disc insertion picks up the patched key automatically.

(define %arm-config-patch-script
  (program-file "arm-config-patch"
    #~(begin
        (use-modules (ice-9 textual-ports) (srfi srfi-13))
        ;; arm.yaml is guaranteed present (activation seeded it if absent).
        ;; sops-secrets is a shepherd requirement so secrets are decrypted by now.
        ;; Short retry guards against the rare case where the kernel hasn't
        ;; flushed the secret file to disk yet when this process starts.
        (let loop ((n 10))
          (cond
            ((zero? n)
             (format (current-error-port)
                     "arm-config-patch: /run/secrets/tmdb/api_key not found~%")
             (primitive-exit 1))
            ((file-exists? "/run/secrets/tmdb/api_key") #t)
            (else (sleep 1) (loop (- n 1)))))
        (define (patch-yaml-key content key new-val)
          (let ((prefix (string-append key ": ")))
            (string-join
             (map (lambda (line)
                    (if (string-prefix? prefix line)
                        (string-append prefix "\"" new-val "\"")
                        line))
                  (string-split content #\newline))
             "\n")))
        (let* ((arm-yaml  "/data/arm/arm.yaml")
               (tmdb-key  (string-trim-right
                           (call-with-input-file "/run/secrets/tmdb/api_key"
                                                 get-string-all)))
               (content   (call-with-input-file arm-yaml get-string-all))
               (patched   (patch-yaml-key
                           (patch-yaml-key
                            (patch-yaml-key
                             ;; NLMeans light denoise: removes film grain/speckle
                             ;; before NVENC sees it. CPU denoise + GPU encode run
                             ;; in parallel so throughput is barely affected.
                             ;; Preserve ARM's default subtitle flags alongside.
                             (patch-yaml-key
                              (patch-yaml-key
                               (patch-yaml-key
                                (patch-yaml-key content
                                               "TMDB_API_KEY" tmdb-key)
                                "METADATA_PROVIDER" "tmdb")
                               "HB_PRESET_BD" "H.265 NVENC 1080p")
                              "HB_PRESET_DVD" "H.265 NVENC 480p30")
                             "HB_ARGS_BD" "--subtitle scan -F --subtitle-burned --audio-lang-list eng --all-audio --nlmeans=light")
                            "HB_ARGS_DVD" "--subtitle scan -F --nlmeans=light")
                           "BASH_SCRIPT" "/etc/arm/config/post-process.sh")))
          (call-with-output-file arm-yaml
            (lambda (p) (display patched p)))
          (format #t "arm-config-patch: patched TMDB_API_KEY, METADATA_PROVIDER, HB_PRESET_BD, HB_PRESET_DVD, HB_ARGS_BD, HB_ARGS_DVD, BASH_SCRIPT~%")))))

(define edison-arm-config-patch-service
  (list
   (simple-service 'arm-config-patch
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (provision '(arm-config-patch))
                     (requirement '(sops-secrets))
                     (one-shot? #t)
                     (start #~(make-forkexec-constructor
                               (list #$%arm-config-patch-script)
                               #:log-file "/var/log/arm-config-patch.log"))
                     (stop #~(make-kill-destructor))
                     (documentation "Patch arm.yaml with TMDB key after sops decrypts secrets."))))))

;;;
;;; Host udev rule to trigger ARM rip when a disc is inserted.
;;;
;;; The ARM container can't receive kernel NETLINK udev events (they are
;;; scoped to the network namespace, which Pasta isolates).  Instead, we add
;;; a host udev rule that fires when the kernel reports a media-change event
;;; on an optical drive, then calls podman exec to invoke the ARM ripper
;;; inside the running container.
;;;
;;; The trigger script is written to /run/arm-trigger.sh by the data-dir
;;; activation service and referenced from the udev rule.  (We use /run
;;; because it is always writable at activation time; /usr/local/bin would
;;; need a separate store derivation.)

(define %arm-udev-rules
  (udev-rule "90-arm-disc-trigger.rules"
             ;; Fire on optical drive media-change events only when media is present.
             ;; RUN writes a background job to avoid blocking udev's 60s timeout.
             ;; %k expands to the kernel device name (e.g. sr1).
             (string-append
              "SUBSYSTEM==\"block\", KERNEL==\"sr[0-9]*\", ACTION==\"change\","
              " ENV{ID_CDROM_MEDIA}==\"1\","
              " RUN+=\"/bin/sh -c '/run/arm-trigger.sh %k &'\"\n")))

(define edison-arm-udev-service
  (list (simple-service 'arm-disc-trigger-udev
                        udev-service-type
                        (list %arm-udev-rules))))

;;;
;;; sops-guix: decrypt Tailscale auth keys to /run/secrets/ at boot
;;;

(define edison-sops-service
  (list
   (service sops-secrets-service-type
            (sops-service-configuration
             (sops sops)
             (gnupg-home "/var/lib/sops")
             (secrets
              (list
               ;; Tailscale auth keys — #o444 so rootless containers can read them
               (sops-secret (key '("tailscale" "jellyfin_authkey"))
                            (file %sops-edison)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "navidrome_authkey"))
                            (file %sops-edison)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "arm_authkey"))
                            (file %sops-edison)
                            (permissions #o444))
               ;; MakeMKV beta/purchased license key — read at activation to
               ;; write /data/arm/.MakeMKV/settings.conf for the ARM container.
               (sops-secret (key '("makemkv" "license_key"))
                            (file %sops-edison)
                            (permissions #o400))
               ;; TMDB API key — written into arm.yaml at activation for title lookup.
               (sops-secret (key '("tmdb" "api_key"))
                            (file %sops-edison)
                            (permissions #o400))))))))  ; root-only: written to arm.yaml at activation

;;;
;;; OCI container helpers — reuse make-ts-sidecar / make-app-container
;;; from server-services, with Edison's LAN IP as the backend host.
;;;

(define %edison-ip "192.168.88.14")

;;;
;;; Jellyfin — media server with NVIDIA hardware transcoding
;;;
;;; Config/cache on /data/jellyfin; media read-only from /media.
;;; GPU assignment: Quadro P2000 (nvidia0, Pascal, 5 GB) for NVENC/NVDEC.
;;; Quadro M2000 (nvidia1) is reserved for ARM video encoding.
;;; NVIDIA runtime libs are mounted from the host profile so ffmpeg can
;;; dlopen libnvidia-encode.so.1 and libcuda.so.1 for hardware transcode.
;;;

(define %jellyfin-containers
  (list
   (make-ts-sidecar "jellyfin"
                    #:serve-port 8096
                    #:backend-host %edison-ip)
   (make-app-container
    "jellyfin" "jellyfin/jellyfin:latest"
    #:volumes
    (list "/data/jellyfin/config:/config"
          "/data/jellyfin/cache:/cache"
          "/media:/media:ro"
          ;; NVIDIA runtime libs (libnvidia-encode, libcuda, libnvcuvid …)
          ;; dlopen'd by ffmpeg for NVENC/NVDEC hardware transcoding.
          ;; Populated at boot by nvidia-devices with real .so files (cp -L).
          "/var/lib/nvidia-container-libs:/usr/local/nvidia/lib:ro")
    #:environment
    (list "JELLYFIN_DATA_DIR=/config"
          "JELLYFIN_CACHE_DIR=/cache"
          "TZ=Europe/Oslo"
          ;; Prepend the host NVIDIA libs so ffmpeg finds them before any
          ;; stubs that might be bundled in the container image.
          "LD_LIBRARY_PATH=/usr/local/nvidia/lib")
    ;; Wait for NVIDIA device nodes and NFS mount before starting
    #:requirement '(nvidia-devices nfs-media)
    ;; M2000 (nvidia1) only — P2000 (nvidia0) is reserved for ARM encoding.
    ;; nvidia-cap1 = NVENC capability; nvidia-cap2 = general caps.
    ;; nvidiactl and nvidia-uvm are shared across both GPUs.
    #:extra-arguments
    (list "--device=/dev/nvidia1"
          "--device=/dev/nvidiactl"
          "--device=/dev/nvidia-uvm"
          "--device=/dev/nvidia-caps/nvidia-cap1"
          "--device=/dev/nvidia-caps/nvidia-cap2"))))

;;;
;;; Navidrome — Subsonic API for Android clients (DSub, Ultrasonic, etc.)
;;;
;;; Reads music directly from /media/music (same dir as MPD).
;;; Accessible via Tailscale at navidrome.<tailnet>.ts.net.
;;; Also published on LAN at 192.168.88.14:4533 (port 4533 open in nftables)
;;; so Sonos devices can stream directly when Symfonium casts to them.
;;;

(define %navidrome-containers
  (list
   (make-ts-sidecar "navidrome"
                    #:serve-port 4533
                    #:backend-host %edison-ip
                    #:ports '("4533:4533"))
   (make-app-container
    "navidrome" "deluan/navidrome:latest"
    #:volumes
    (list "/data/navidrome:/data"
          "/media/music:/music:ro")
    #:environment
    (list "ND_MUSICFOLDER=/music"
          "ND_DATAFOLDER=/data"
          "ND_LOGLEVEL=info"
          "ND_PORT=4533"
          "TZ=Europe/Oslo")
    ;; Wait for NFS mount (/media from lovelace) before starting
    #:requirement '(nfs-media))))

;;;
;;; Caddy — HTTPS reverse proxy for Navidrome on the LAN
;;;
;;; Provides https://192.168.88.14:4534 → Navidrome (port 4533).
;;; Uses Caddy's built-in self-signed cert (tls internal); enable
;;; "Ignore certificate errors" in Symfonium's server settings.
;;; Set as Symfonium primary connection so Sonos can receive LAN HTTPS
;;; stream URLs; Tailscale HTTPS as secondary for remote access.
;;;

(define %caddy-navidrome-caddyfile
  (plain-file "navidrome-caddyfile"
    (string-append
     %edison-ip ":4534 {\n"
     "    tls internal\n"
     "    reverse_proxy host.containers.internal:4533\n"
     "}\n")))

(define %caddy-navidrome-container
  (oci-container-configuration
   (user "rafael")
   (image "docker.io/library/caddy:latest")
   (provision "caddy-navidrome")
   (requirement '(navidrome podman-prune cgroups2-fs-owner cgroups2-limits
                  rootless-podman-shared-root-fs user-processes))
   (respawn? #t)
   (volumes
    (list (cons %caddy-navidrome-caddyfile "/etc/caddy/Caddyfile:ro")
          "/data/caddy:/data"))
   (ports '("4534:4534"))))

;;;
;;; ARM — Automatic Ripping Machine
;;;
;;; Rips optical discs inserted into /dev/sr0 and /dev/sr1.
;;; Output goes to /media/rips. Config on /data/arm.
;;; Web UI accessible via Tailscale at arm.<tailnet>.ts.net.
;;;

(define %arm-containers
  (list
   (make-ts-sidecar "arm"
                    #:serve-port 8080
                    #:backend-host %edison-ip)
   (make-app-container
    "arm" "automaticrippingmachine/automatic-ripping-machine:latest"
    #:volumes
    (list "/data/arm:/etc/arm/config"
          "/media/rips:/home/arm/media"
          ;; Final Jellyfin-ready destinations (shared with manually-added media)
          "/media/movies:/home/arm/movies"
          "/media/tv:/home/arm/tv"
          ;; Music output: abcde writes ripped CDs here; Navidrome scans it
          "/media/music:/home/arm/Music"
          ;; Persist MakeMKV settings across container restarts.
          ;; The activation service writes the key from sops to
          ;; /data/arm/.MakeMKV/settings.conf; mounting it here makes MakeMKV
          ;; find the key without relying on ARM's internal copy-on-startup.
          "/data/arm/.MakeMKV:/home/arm/.MakeMKV"
          ;; NVIDIA runtime libs for HandBrake NVENC encoding.
          ;; HandBrake dlopen's libnvidia-encode.so.1 and libcuda.so.1 at runtime.
          ;; Populated at boot by nvidia-devices with real .so files (cp -L).
          "/var/lib/nvidia-container-libs:/usr/local/nvidia/lib:ro"
          ;; Host udev database: gives pyudev access to ID_FS_LABEL and
          ;; ID_CDROM_MEDIA_BD so ARM can identify discs without bdmt_eng.xml
          ;; (falls back to the disc's volume label for TMDB lookup).
          ;; Read-only so the container cannot modify host udev state.
          "/run/udev:/run/udev:ro"
          ;; Patched identify.py: adds MakeMKV CINFO:2 and bdmt_*.xml language
          ;; fallbacks for discs that lack bdmt_eng.xml (e.g. old TV show BDs).
          ;; Not :ro because arm_user_files_setup.sh chowns files at startup.
          "/data/arm/identify.py:/opt/arm/arm/ripper/identify.py"
          ;; Patched system_drives.py: fixes {err:s} TypeError bug in _tray_status
          ;; and returns CDS_DISC_OK for sr* devices when CDROM_DRIVE_STATUS ioctl
          ;; fails (rootless podman devtmpfs presents sr* as regular-file stubs).
          "/data/arm/system_drives.py:/opt/arm/arm/models/system_drives.py"
          ;; Patched music_brainz.py: adds GNUDB (freedb-compatible) fallback when
          ;; MusicBrainz returns 404 for a disc.  Also ensures no_of_titles is
          ;; always set so the ARM UI shows CD rip progress (track N / total).
          "/data/arm/music_brainz.py:/opt/arm/arm/ripper/music_brainz.py")
    #:environment
    (list "TZ=Europe/Oslo"
          ;; PUID=0: run as container root, which rootless Podman maps to
          ;; host uid 1001 (rafael). Needed because NFS uid mapping is numeric
          ;; and the arm user (container uid 1000) maps to host subuid ~232071
          ;; which has no write permission on the NFS-mounted media dirs.
          "PUID=0"
          "PGID=0"
          ;; Host NVIDIA libs for HandBrake NVENC (libnvidia-encode, libcuda).
          "LD_LIBRARY_PATH=/usr/local/nvidia/lib")
    ;; Wait for NVIDIA device nodes, NFS mount, and TMDB key patch before starting.
    ;; arm-config-patch must run before ARM imports config.py, otherwise ARM's
    ;; merge-and-writeback overwrites the TMDB key with the template's empty value.
    #:requirement '(nvidia-devices nfs-media arm-config-patch)
    ;; Pass both optical drives into the container.
    ;; --group-add=keep-groups carries the host user's supplementary groups
    ;; (including 'cdrom') into the container so the cdrom block devices
    ;; (root:cdrom 660) remain accessible despite rootless uid mapping.
    ;; M2000 (nvidia1) reserved for HandBrake NVENC encoding.
    ;; nvidia-cap1 is the NVENC capability device (made world-readable by
    ;; the nvidia-devices service above).
    #:extra-arguments
    (list "--device=/dev/sr0"
          "--device=/dev/sr1"
          ;; SCSI generic devices: MakeMKV uses /dev/sg* (not /dev/sr*) to
          ;; enumerate and communicate with optical drives.  Without these,
          ;; makemkvcon reports "can't find any usable optical drives".
          "--device=/dev/sg0"
          "--device=/dev/sg1"
          ;; ARM mounts the disc at /mnt/dev/sr0 (per its fstab) to inspect
          ;; the filesystem for BDMV/VIDEO_TS directories and identify whether
          ;; the disc is Blu-ray, DVD, or data.  Mounting a block device inside
          ;; a rootless container requires both SYS_ADMIN and seccomp=unconfined
          ;; (rootless Podman's seccomp policy blocks the mount(2) syscall even
          ;; when SYS_ADMIN is granted).
          "--cap-add=SYS_ADMIN"
          "--security-opt=seccomp=unconfined"
          ;; Overlay the container's udev rules directory with an empty tmpfs.
          ;; 51-docker-arm.rules inside the image (symlink → /opt/arm/setup/)
          ;; would otherwise fire via the container's udevd when /run/udev is
          ;; mounted, double-triggering rips alongside our host arm-trigger.sh.
          ;; An empty tmpfs silences all container udev rules while leaving
          ;; pyudev property reads (via /run/udev) fully functional.
          "--tmpfs=/lib/udev/rules.d"
          "--group-add=keep-groups"
          "--device=/dev/nvidia0"
          "--device=/dev/nvidiactl"
          "--device=/dev/nvidia-uvm"
          "--device=/dev/nvidia-caps/nvidia-cap1"
          "--device=/dev/nvidia-caps/nvidia-cap2"))))

;;;
;;; Single oci-service-type for all Edison containers
;;;

(define edison-container-services
  (append
   ;; Gate services: one-shot readiness checks that ensure each ts-* sidecar
   ;; container is registered in podman before the app container tries
   ;; --network=container:ts-<name>.  Without these, app containers race
   ;; against their sidecar's `podman run` and fail with exit 126.
   (list (make-ts-ready-service "jellyfin")
         (make-ts-ready-service "navidrome")
         (make-ts-ready-service "arm"))
   (list
    (service oci-service-type
             (oci-configuration
              (runtime 'podman)
              (containers (append %jellyfin-containers
                                  %navidrome-containers
                                  (list %caddy-navidrome-container)
                                  %arm-containers)))))))
