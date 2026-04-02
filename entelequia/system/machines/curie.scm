(define-module (entelequia system machines curie)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib common-services)
  #:use-module (entelequia systems desktop)  ; For desktop-home-services
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles networking)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services base)     ; guix-extension, guix-service-type
  #:use-module (gnu services xorg)
  #:use-module (gnu services containers)
  #:use-module (gnu system accounts)
  #:use-module (xlibre))

(use-service-modules xorg containers)

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

;;; AMD Xlibre configuration
;;; Using modesetting driver instead of xlibre-video-amdgpu for better pixmap stability
;;; Modesetting provides better per-CRTC framebuffer handling to prevent pixmap corruption
;;; TearFree option enabled at driver level for smooth rendering
;;; See: https://wiki.archlinux.org/title/AMDGPU#Xorg_configuration
;;; Rollback if needed: sudo guix system roll-back

(define amd-xlibre-config
  (xlibre-configuration
   (modules (list xlibre-input-libinput))  ; Removed xlibre-video-amdgpu module
   (drivers '("modesetting"))               ; Use modesetting driver only
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (extra-config
    (list "Section \"Device\""
          "  Identifier \"AMD Graphics\""
          "  Driver \"modesetting\""
          "  Option \"TearFree\" \"true\""
          "EndSection"))))

;;; Curie-specific packages

(define curie-extra-packages
  (append
   (specifications->packages amd-specific-packages)
   (specifications->packages curie-specific-packages)))

;;; Curie system definition

;; Define curie-specific services
(define curie-services
  (append
   (list
    ;; Game controller udev rules (PS4, PS5, Xbox, etc.)
    gamepad-udev-rules-service

    ;; Allow non-bonded Bluetooth HID devices (PS5 DualSense, etc.)
    bluetooth-input-config-service

    ;; Rootless podman for containerization
    (service rootless-podman-service-type
             (rootless-podman-configuration
              (subuids (list (subid-range (name "rafael"))))
              (subgids (list (subid-range (name "rafael"))))))

    ;; Guix Home configuration
    (guix-home-config
     curie-config
     (home-environment
      ;; Include profile packages
      (packages (append (base-home-packages)
                        (development-home-packages)
                        (networking-home-packages)
                        email-home-packages
                        documentation-home-packages
                        (gaming-home-packages)))
      ;; desktop-home-services includes DataLocker service
      (services desktop-home-services)))

    ;; SLiM display manager with AMD Xlibre config
    (service slim-service-type
             (slim-configuration
              (auto-login? #f)
              (default-user "rafael")
              (xorg-configuration amd-xlibre-config))))

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
                     (chroot-directories '("/var/cache/slicer-ccache")))))))

(define curie-system
  (operating-system
   (inherit (make-desktop-base-os curie-config
                                  #:extra-packages curie-extra-packages
                                  #:extra-services curie-services
                                  #:firewall-extra-tcp-ports '(4549)
                                  #:firewall-extra-udp-ports '(4549 4171 4175 4179)
                                  #:firewall-trusted-subnets '("192.168.88.0/24")))

   ;; Curie-specific kernel arguments (amd_pstate, network interface naming)
   (kernel-arguments (gpu-kernel-arguments 'amd
                                           #:extra-args '("net.ifnames=0"
                                                          "biosdevname=0")))

   ;; User configuration (add cgroup to supplementary groups for containers)
   ;; Note: cgroup group now defined in base.scm
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  ;; Include all base groups + cgroup (containers)
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "realtime" "audio" "video" "cgroup")))
                 %base-user-accounts))

   ;; Bootloader configuration
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))))

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
