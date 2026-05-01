(define-module (entelequia system machines hopper)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia systems desktop)  ; For desktop-home-services
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles python-learning)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services guix)       ; guix-home-service-type
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)         ; thermald
  #:use-module (gnu services containers)
  #:use-module (gnu system accounts)
  #:use-module (xlibre)
  #:export (hopper-os))

(use-service-modules xorg containers pm)

;;; Hopper system configuration
;;;
;;; Work laptop (Dell XPS 13) with Intel integrated graphics.
;;; Inherits from desktop-base.  Modelled on alucard's structure
;;; (multi-profile home, slim + bspwm) but with single user, Intel
;;; xorg/thermald, and laptop-mode services.

;;; Machine configuration

(define hopper-config
  (machine-config
   (hostname "hopper")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl"))
   (gpu-type 'intel)
   (machine-type 'laptop)))

;;; Intel xorg configuration
;;;
;;; Use modesetting (KMS) like curie's AMD config — works well for
;;; modern Intel iGPUs and avoids the legacy xlibre-video-intel
;;; driver.  TearFree for smoother rendering.

(define intel-xlibre-config
  (xlibre-configuration
   (modules (list xlibre-input-libinput))
   (drivers '("modesetting"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   (extra-config
    (list "Section \"Device\""
          "  Identifier \"Intel Graphics\""
          "  Driver \"modesetting\""
          "  Option \"TearFree\" \"true\""
          "EndSection"))))

;;; Hopper-specific packages
;;
;; intel-microcode: CPU vulnerability mitigations + errata fixes.
;; tlp / powertop: laptop power tooling so tlp-stat / powertop work
;; system-wide (TLP service is enabled via machine-type 'laptop).
;; lm-sensors: provides the `sensors` CLI for thermal/voltage probes.

(define hopper-extra-packages
  (append
   (specifications->packages '("intel-microcode"
                               "tlp"
                               "powertop"
                               "lm-sensors"))
   (specifications->packages curie-specific-packages)))

;;; Home environments

(define rafael-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (development-home-packages)
                     (gaming-home-packages)
                     email-home-packages
                     documentation-home-packages))
   (services desktop-home-services)))

;; Adrian: minimal desktop env (bspwm + dotfiles + games).
;; No dev/email/docs — keeps the surface small.
;; Filter out age-inappropriate titles (TAB has graphic content).
(define adrian-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (python-learning-home-packages)
                     (gaming-home-packages
                      ;; Exclude TAB (age).
                      ;; Native CoQ broken on Mesa 25.2.3 + Unity 2021 (same bug as curie's
                      ;; gfx1150 — confirmed Intel UHD 620 also affected).  Use wine variant.
                      #:exclude '("they-are-billions"
                                  "caves-of-qud-native"))))
   (services desktop-home-services)))

;;; Hopper-specific services

(define hopper-services
  (list
   ;; Intel thermal management (was removed from base when AMD took over;
   ;; re-add here for Intel laptops).
   (service thermald-service-type)

   ;; Rootless podman for containerization (rafael)
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subuids (list (subid-range (name "rafael"))))
             (subgids (list (subid-range (name "rafael"))))))

   ;; Guix Home for rafael (full) + adrian (minimal)
   (service guix-home-service-type
            `(("rafael" ,rafael-home-env)
              ("adrian" ,adrian-home-env)))

   ;; SLiM display manager with Intel xorg
   (service slim-service-type
            (slim-configuration
             (auto-login? #f)
             (default-user "rafael")
             (xorg-configuration intel-xlibre-config)))))

(define hopper-os
  (operating-system
   (inherit (make-desktop-base-os hopper-config
                                  #:extra-packages hopper-extra-packages
                                  #:extra-services hopper-services
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file
                                              "hopper-deploy.pub"
                                              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJKoGmzFMaiX/JdtOXJjejf0X7gjG++0qF3uEJWCOrfu hopper-deploy [A]:0x0266C7CE")))))

   ;; Intel kernel arguments + laptop power tweaks for the XPS 13 (Kaby Lake R)
   ;;   i915.enable_psr=1     Panel Self Refresh — saves power on stable display
   ;;   i915.enable_fbc=1     Frame Buffer Compression — reduces memory bandwidth
   ;;   pcie_aspm=force       Aggressive PCIe Active State Power Management
   ;;   mem_sleep_default=deep  Use deep S3 sleep (when supported) instead of s2idle
   (kernel-arguments (gpu-kernel-arguments
                      'intel
                      #:extra-args '("i915.enable_psr=1"
                                     "i915.enable_fbc=1"
                                     "pcie_aspm=force"
                                     "mem_sleep_default=deep")))

   ;; Users: rafael (admin, podman) + adrian (account-only, occasional games)
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "realtime" "audio" "video" "cgroup")))
                 (user-account
                  (name "adrian")
                  (comment "Adrian")
                  (group "users")
                  (home-directory "/home/adrian")
                  (supplementary-groups '("netdev" "audio" "video")))
                 %base-user-accounts))

   ;; Bootloader (UEFI)
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Swap (nvme0n1p2)
   (swap-devices (list (swap-space
                        (target (uuid "d84471cc-e5b5-494d-8ef4-5c13cbca038c")))))

   ;; File systems (nvme0n1p1 EFI + nvme0n1p3 root)
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "6996-E48F" 'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid
                                  "513d801c-db9a-448a-a159-7a16818b1f15"
                                  'ext4))
                         (type "ext4"))
                        %base-file-systems))))

hopper-os
