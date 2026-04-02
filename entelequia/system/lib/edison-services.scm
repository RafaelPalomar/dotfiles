(define-module (entelequia system lib edison-services)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services audio)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages base)
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
            edison-sops-service
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
                          "/data/navidrome"))
                       ;; /data/arm: owned by container uid 1000 of the ARM container.
                       ;; In rootless Podman (rafael uid 1001, subuid starts at 231072):
                       ;;   container uid 0  = host uid 1001 (rafael)
                       ;;   container uid N  = host uid 231072 + N - 1  (for N >= 1)
                       ;; ARM runs its 'arm' user as uid 1000 inside the container,
                       ;; which maps to host uid 232071 (231072 + 1000 - 1).
                       (mkdir-p "/data/arm")
                       (chown "/data/arm" 232071 232071)
                       (chmod "/data/arm" #o755)
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
             ;; Wait for NFS mount (/media) and network before starting
             (shepherd-requirement '(file-systems networking))
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
          (system* #$(file-append coreutils "/bin/chgrp") "video" "/dev/nvidiactl")))))

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
                            (permissions #o444))))))))

;;;
;;; OCI container helpers — reuse make-ts-sidecar / make-app-container
;;; from server-services, with Edison's LAN IP as the backend host.
;;;

(define %edison-ip "192.168.88.14")

;;;
;;; Jellyfin — media server with NVIDIA hardware transcoding
;;;
;;; Config/cache on /data/jellyfin; media read-only from /media.
;;; NVIDIA devices passed via --device for NVENC transcoding.
;;; Tailscale sidecar exposes Jellyfin on Tailscale HTTPS.
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
          "/media:/media:ro")
    #:environment
    (list "JELLYFIN_DATA_DIR=/config"
          "JELLYFIN_CACHE_DIR=/cache"
          "TZ=Europe/Oslo")
    ;; Wait for NVIDIA device nodes and NFS mount before starting
    #:requirement '(nvidia-devices file-system-/media)
    ;; NVIDIA device passthrough for NVENC transcoding.
    ;; Requires rafael in "video" group (set in edison.scm user-account).
    ;; The nvidia-uvm device is created lazily; the NVIDIA module must be
    ;; loaded before starting Jellyfin (nonguix-transformation-nvidia handles this).
    #:extra-arguments
    (list "--device=/dev/nvidia0"
          "--device=/dev/nvidia1"
          "--device=/dev/nvidiactl"
          "--device=/dev/nvidia-uvm"))))   ; nvidia-modeset not present on Quadro P2000/M2000

;;;
;;; Navidrome — Subsonic API for Android clients (DSub, Ultrasonic, etc.)
;;;
;;; Reads music directly from /media/music (same dir as MPD).
;;; Accessible via Tailscale at navidrome.<tailnet>.ts.net.
;;;

(define %navidrome-containers
  (list
   (make-ts-sidecar "navidrome"
                    #:serve-port 4533
                    #:backend-host %edison-ip)
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
    #:requirement '(file-system-/media))))

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
          ;; Music output: abcde writes ripped CDs here; Navidrome scans it
          "/media/music:/home/arm/Music")
    #:environment
    (list "TZ=Europe/Oslo"
          ;; PUID=0: run as container root, which rootless Podman maps to
          ;; host uid 1001 (rafael). Needed because NFS uid mapping is numeric
          ;; and the arm user (container uid 1000) maps to host subuid ~232071
          ;; which has no write permission on the NFS-mounted media dirs.
          "PUID=0"
          "PGID=0")
    ;; Wait for NFS mount (/media from lovelace) before starting
    #:requirement '(file-system-/media)
    ;; Pass both optical drives into the container
    #:extra-arguments
    (list "--device=/dev/sr0"
          "--device=/dev/sr1"))))

;;;
;;; Single oci-service-type for all Edison containers
;;;

(define edison-container-services
  (list
   (service oci-service-type
            (oci-configuration
             (runtime 'podman)
             (containers (append %jellyfin-containers
                                 %navidrome-containers
                                 %arm-containers))))))
