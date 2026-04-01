(define-module (entelequia system machines edison)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers server-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib server-services)
  #:use-module (entelequia system lib edison-services)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles server)
  #:use-module (entelequia systems server)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services guix)
  #:use-module (gnu services networking)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages containers)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (nonguix transformations)
  #:use-module (guix gexp)
  #:export (edison-os))

(use-service-modules containers networking)

;;; Edison system configuration
;;;
;;; Headless multimedia server. Two NVIDIA Quadro GPUs (P2000 + M2000) for
;;; NVENC hardware transcoding in Jellyfin. Two optical drives for ARM.
;;; Media stored on lovelace:/data/media, NFS-mounted as /media.
;;; Local data disk (XFS, /dev/sdb1) mounted at /data.
;;;
;;; Hardware: Intel Xeon E5-1620 @ 3.60GHz, 15GB RAM.
;;; Boot mode: EFI. Network interface: enp0s25.
;;; GPUs: NVIDIA Quadro P2000 (03:00) + Quadro M2000 (08:00).

;;; Machine configuration

(define edison-config
  (machine-config
   (hostname "edison")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl"))
   (gpu-type 'nvidia)
   (machine-type 'server)))

;;; Edison-specific extra packages

(define edison-extra-packages
  (append
   (specifications->packages (gpu-driver-packages 'nvidia))
   (specifications->packages edison-specific-packages)))

;;; Home environment — minimal server setup

(define edison-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (server-home-packages)))
   (services server-home-services)))

;;; Edison-specific services

(define edison-services
  (append
   ;; rootless Podman: creates cgroup group, configures subids for rafael,
   ;; and provides shepherd services that oci containers depend on.
   ;; (podman #f): suppress podman from this service's profile extension —
   ;; oci-service-type already adds it. The NVIDIA replace-mesa transformation
   ;; modifies packages in the configuration record but NOT in oci-service-type's
   ;; extension lambda (which uses the module-level podman directly), causing two
   ;; different podman derivations if both add it to the profile.
   (list (service rootless-podman-service-type
                  (rootless-podman-configuration
                   (podman #f))))
   ;; sops: decrypt Tailscale auth keys to /run/secrets/ at boot.
   ;; Requires /var/lib/sops GPG key and sops/edison.yaml to exist.
   edison-sops-service
   ;; Create /data subdirectories at activation time
   edison-data-dir-service
   ;; Remove stale containers from previous boot before OCI services start
   podman-prune-service
   ;; NVIDIA device nodes — trigger udev rules at boot to create /dev/nvidia*
   ;; Workaround for 90-nvidia.rules TEST!="/dev/nvidia-uvm" race condition
   edison-nvidia-devices-service
   ;; MPD music daemon (port 6600 MPD protocol, port 8000 HTTP stream)
   edison-mpd-service
   ;; OCI containers: Jellyfin, Navidrome, ARM
   edison-container-services
   ;; Guix Home
   (list (service guix-home-service-type
                  `(("rafael" ,edison-home-env))))))

;;; Edison operating system

(define edison-system
  (operating-system
   (inherit (make-server-base-os
             edison-config
             #:extra-packages  edison-extra-packages
             #:extra-services  edison-services
             #:ssh-authorized-keys
             `(("root"   ,(plain-file "edison-deploy.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINjd0lrTPhG75R5jWBCrtN7xX4u7D12527agB+Jolx9f openpgp:0xA08C8C2F"))
               ("rafael" ,(plain-file "edison-deploy-rafael.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINjd0lrTPhG75R5jWBCrtN7xX4u7D12527agB+Jolx9f openpgp:0xA08C8C2F")))
             ;; Ports: 8096 Jellyfin, 4533 Navidrome, 8080 ARM web UI, 6600 MPD, 8000 MPD HTTP stream
             #:firewall-extra-tcp-ports
             '(8096   ; Jellyfin (LAN fallback)
               4533   ; Navidrome (LAN fallback)
               8080   ; ARM web UI (LAN fallback)
               6600   ; MPD protocol
               8000)  ; MPD HTTP stream
             #:firewall-extra-udp-ports '()))

   ;; NVIDIA kernel arguments: blacklist nouveau, enable DRM modesetting
   (kernel-arguments (gpu-kernel-arguments 'nvidia))

   ;; User — "video" group for NVIDIA device access in rootless Podman,
   ;; "cdrom" for optical drives (ARM container passthrough)
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "cgroup" "video" "cdrom")))
                 (user-account
                  (name "oci-container")
                  (group "users")
                  (system? #t)
                  (comment "OCI services account")
                  (home-directory "/home/oci-container")
                  (shell (file-append shadow "/sbin/nologin"))
                  (supplementary-groups '("cgroup")))
                 %base-user-accounts))

   ;; EFI bootloader
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Swap
   (swap-devices (list (swap-space
                        (target (uuid "15f34a5c-da1b-435d-b0fa-7e684328c302")))))

   ;; File systems
   ;; Root: /dev/sda3 (ext4, 927GB)
   ;; EFI:  /dev/sda1 (vfat, 1GB)
   ;; Swap: /dev/sda2 (3.7GB)
   ;; Data: /dev/sdb1 (xfs, 931GB) — local storage for container data
   ;; Media: lovelace:/data/media (NFS) — large media library
   (file-systems (cons* (file-system
                          (mount-point "/")
                          (device (uuid "a1d2e7e4-df27-449e-bc1c-b74683c5765e" 'ext4))
                          (type "ext4"))
                        (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "AE97-DD7E" 'fat32))
                          (type "vfat"))
                        (file-system
                          (mount-point "/data")
                          (device (uuid "8db6fed7-c790-4d82-96b2-82fada6d42d7" 'xfs))
                          (type "xfs")
                          (options "noatime,lazytime")
                          (mount-may-fail? #t))
                        (file-system
                          (mount-point "/media")
                          (device "192.168.88.46:/data/media")
                          (type "nfs")
                          ;; noatime + large rsize/wsize for media streaming performance
                          ;; soft: give up after timeo*retrans deciseconds (30*2=6s) instead of hanging
                          ;; intr: allow SIGINT to interrupt hung NFS calls (prevents total system hang)
                          (options "noatime,rsize=131072,wsize=131072,vers=4,soft,intr,timeo=30,retrans=2")
                          ;; Boot even if lovelace is unreachable at mount time
                          (mount-may-fail? #t)
                          (check? #f))
                        %base-file-systems))))

;;; Apply NVIDIA transformation (headless: no X11 config) and export

(define edison-os
  ((nonguix-transformation-nvidia #:configure-xorg? #f) edison-system))

edison-os
