(define-module (entelequia system machines curie)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia packages latex)
  #:use-module (entelequia system lib common-services)
  #:use-module (entelequia system lib pam-gnupg)
  #:use-module (entelequia system lib chromium-policy)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)     ; guix-extension, guix-service-type
  #:use-module (gnu services shepherd)   ; shepherd-service, shepherd-root-service-type
  #:use-module (guix gexp)               ; #~ for shepherd start gexps
  #:use-module (sops packages sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops))

;;; Curie system configuration
;;;
;;; Laptop system with AMD GPU. Inherits from desktop-base
;;; and adds AMD-specific configuration.

;;; Machine configuration

(define curie-config
  (machine-config
   (hostname "curie")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (gpu-type 'amd)
   (machine-type 'laptop)))

;;; Curie-specific packages

(define curie-extra-packages
  (append
   (specifications->packages workstation-packages)
   (specifications->packages base-latex-packages)
   (list font-sciflycore-sans latex-nfr)))

;;; Curie system definition

;; Define curie-specific services
;;; SOPS encrypted secrets file (in git, encrypted). Decrypted at boot by the
;;; Curie SOPS key in /root/.gnupg (passwordless, generated on the host).
(define %sops-curie
  (local-file "../../../sops/curie.yaml"))

(define curie-services
  (append
   (list
    ;; sops-guix: decrypt rafael's OpenRouter key to /run/secrets/ at boot.
    ;; openrouter/rafael -> /run/secrets/openrouter/rafael (owner rafael, 0400),
    ;; read by the alpha launcher (rafael's home service).
    (service sops-secrets-service-type
             (sops-service-configuration
              (sops sops)
              (gnupg-home "/root/.gnupg")
              (secrets
               (list (sops-secret (key '("openrouter" "rafael"))
                                  (file %sops-curie)
                                  (user "rafael")
                                  (permissions #o400))))))

    ;; Game controller udev rules (PS4, PS5, Xbox, etc.)
    gamepad-udev-rules-service

    ;; DDC/CI monitor control: let the `i2c' group drive /dev/i2c-* so
    ;; ddcutil (external monitor brightness/input, e.g. the OUS EIZO on DP)
    ;; works without sudo.  rafael is added to `i2c' via extra-user-groups.
    i2c-ddc-udev-rules-service

    ;; Allow non-bonded Bluetooth HID devices (PS5 DualSense, etc.)
    bluetooth-input-config-service

    ;; Thunderbolt device manager (boltd).  Security level is `user', so
    ;; the ThinkPad Thunderbolt 4 Dock must be authorized on every connect;
    ;; boltd persists a per-UUID enrollment and re-authorizes it
    ;; automatically.  One-time after first reconfigure:
    ;;   boltctl enroll <uuid>   (uuid from `boltctl list')
    (service bolt-service-type)

    ;; Home environment for rafael lives in
    ;; entelequia/home/machines/curie-rafael.scm and is deployed
    ;; independently via `guix home reconfigure' (alias `home-reconfigure').

    ;; pam-gnupg: SLiM login password → gpg-agent passphrase cache.
    ;; Eliminates pinentry prompts for keygrips listed in ~/.pam-gnupg.
    ;; Requires the GPG passphrase to equal the login password.
    (service pam-gnupg-service-type)

    ;; Chromium managed policy: SearXNG default search + Bitwarden forcelist.
    ;; Per-profile launchers come from the home environment.
    chromium-policy-service)

   ;; zram compressed swap (8GB, zstd compression)
   (zram-service #:size-mb 8192)

   ;; -----------------------------------------------------------------------
   ;; ccache for Slicer development
   ;; -----------------------------------------------------------------------
   ;; Expose /var/cache/slicer-ccache inside the Guix build sandbox so that
   ;; slicer-5.8 (and its loadable-module packages) can use a persistent
   ;; compiler cache across derivation rebuilds.  Without this the daemon
   ;; sandbox would hide the directory and every patch-tweak rebuild starts
   ;; cold.
   ;;
   ;; The directory is created by activation (runs during 'guix system
   ;; reconfigure') as a world-writable sticky directory (like /tmp) so that
   ;; any guixbuilder* UID can write to it.
   ;;
   ;; Usage after reconfigure:
   ;;   - Guix builds: automatic (slicer.scm detects /var/cache/slicer-ccache)
   ;;   - Personal builds:
   ;;       CCACHE_DIR=/var/cache/slicer-ccache \
   ;;       CCACHE_BASEDIR=$(pwd) \
   ;;       cmake -DCMAKE_C_COMPILER_LAUNCHER=ccache ...

   (list
    (simple-service 'slicer-ccache-dir
                    activation-service-type
                    #~(let ((dir "/var/cache/slicer-ccache"))
                        (unless (file-exists? dir)
                          (mkdir dir))
                        ;; World-writable + sticky: guixbuilder* UIDs can write;
                        ;; sticky bit prevents one builder from removing another's files.
                        (chmod dir #o1777)))

    (simple-service 'guix-daemon-slicer-ccache
                    guix-service-type
                    (guix-extension
                     (chroot-directories '("/var/cache/slicer-ccache"))))

    ;; AMD GPU performance-level toggle for gaming.
    ;;
    ;; Strix Halo gfx1150 on Mesa 25.2 + kernel 6.18 doesn't ramp the
    ;; iGPU sclk under load — stays at 600 MHz / 2799 MHz max even at
    ;; 100% GPU busy + platform_profile=performance + AC connected.
    ;; The workaround is to write "high" to
    ;; /sys/class/drm/card0/device/power_dpm_force_performance_level,
    ;; but that file is root-only by default.
    ;;
    ;; Grant the `video' group write access at boot so that game
    ;; launchers (entelequia/packages/games.scm `make-proton-game-launcher'
    ;; with #:gpu-boost? #t) can toggle "high" before exec and revert
    ;; to "auto" on game exit — same pattern as feral-interactive's
    ;; gamemoded.  rafael is in `video', so no setuid helper or sudo.
    ;;
    ;; Brief sustained max clock during a game is safe; permanently
    ;; pinning high stresses the driver and has caused full-system
    ;; crashes on this silicon — hence the per-game toggle rather than
    ;; a kernel-arg or boot-time `echo high`.
    ;;
    ;; Implementation: one-shot shepherd service.  sysfs perms reset on
    ;; every boot, so activation-service-type isn't enough — must run
    ;; after the amdgpu driver has populated the file (i.e., post-boot).
    (simple-service 'amd-gpu-perf-perms
                    shepherd-root-service-type
                    (list
                     (shepherd-service
                      (documentation "Grant `video' group write access to AMD GPU power_dpm_force_performance_level so game launchers can toggle high/auto without sudo.")
                      (provision '(amd-gpu-perf-perms))
                      (requirement '(file-systems))
                      (one-shot? #t)
                      (start
                       #~(lambda ()
                           (let ((f "/sys/class/drm/card0/device/power_dpm_force_performance_level"))
                             (when (file-exists? f)
                               (chmod f #o664)
                               (let ((video-gid (vector-ref (getgrnam "video") 2)))
                                 (chown f 0 video-gid))))
                           #t))))))))

(define curie-system
  (operating-system
   (inherit (make-desktop-base-os curie-config
                                  #:extra-packages curie-extra-packages
                                  #:extra-services curie-services
                                  ;; 57165 — Barony direct-IP host port (TCP+UDP)
                                  #:firewall-extra-tcp-ports '(4549 57165)
                                  #:firewall-extra-udp-ports '(4549 4171 4175 4179 57165)
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:extra-user-groups '("cgroup" "dialout" "i2c")
                                  ;; Curie-specific kernel arguments
                                  ;;   net.ifnames / biosdevname — keep classic eth0/wlan0 naming
                                  ;;   acpi.ec_no_wakeup=1 — Strix/Krackan Point s2idle wake fix.
                                  ;;     The EC fires spurious wake events that stall resume on
                                  ;;     AMD ThinkPads ("suspends but won't wake").
                                  ;;   amdgpu.cwsr_enable=0 — workaround for a Strix Halo gfx1150 /
                                  ;;     Radeon 890M kernel bug: under sustained GPU load (NMS via
                                  ;;     Proton-GE/DXVK) the MES wedges on Ring 13 and amdgpu's
                                  ;;     reset hard powers-off the SoC.  Reported on amd-gfx for
                                  ;;     6.18; still present in 6.19-rc3.  Revisit when the kernel
                                  ;;     carries the gfx1150 MES fix:
                                  ;;     https://lists.freedesktop.org/archives/amd-gfx/2025-December/135310.html
                                  ;;     https://lists.freedesktop.org/archives/amd-gfx/2025-December/136016.html
                                  ;; Note: resume= intentionally absent — Guix initrd doesn't
                                  ;; resolve resume=UUID=, and the 3.7 GiB swap can't hold a
                                  ;; 30 GiB hibernation image anyway.
                                  #:extra-kernel-arguments '("net.ifnames=0"
                                                             "biosdevname=0"
                                                             "acpi.ec_no_wakeup=1"
                                                             "amdgpu.cwsr_enable=0")))

   ;; Swap device
   (swap-devices (list (swap-space
                        (target (uuid
                                 "a5b672b4-16c3-4f92-836c-01061e66e3fe")))))

   ;; File systems
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "7A6E-89C0"
                                       'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid
                                  "8d5376cc-89ea-4b11-95e8-9908916894f6"
                                  'ext4))
                         (type "ext4"))
                        %base-file-systems))))

curie-system
