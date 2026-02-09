(define-module (entelequia system machines einstein)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system machines datalocker-udev-rules)
  #:use-module (entelequia systems desktop)  ; For desktop-home-services
  #:use-module (entelequia home services emacs)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services containers)
  #:use-module (gnu system accounts)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (nonguix transformations)
  #:use-module (xlibre))

(use-service-modules xorg containers)

;;; Einstein system configuration
;;;
;;; Desktop system with NVIDIA GPU. Inherits from desktop-base
;;; and adds NVIDIA-specific configuration.

;;; Machine configuration

(define einstein-config
  (machine-config
   (hostname "einstein")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (gpu-type 'nvidia)
   (machine-type 'desktop)))

;;; NVIDIA Xorg configuration

(define nvidia-xorg-config
  (xlibre-configuration
   (modules (list nvidia-driver xlibre-input-libinput))
   (drivers '("nvidia"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))))

;;; Einstein-specific packages

(define einstein-extra-packages
  (append
   (specifications->packages nvidia-specific-packages)
   (specifications->packages einstein-specific-packages)))

;;; Einstein system definition

;; Define einstein-specific services
(define einstein-services
  (list
   ;; Rootless podman for containerization
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subuids (list (subid-range (name "rafael"))))
             (subgids (list (subid-range (name "rafael"))))))

   ;; DataLocker Sentry ONE auto-unlock udev rule
   datalocker-udev-rules-service

   ;; Guix Home configuration
   (guix-home-config
    einstein-config
    (home-environment
     ;; Include profile packages
     (packages (append base-home-packages
                       development-home-packages
                       email-home-packages))
     ;; desktop-home-services already includes emacs service
     (services desktop-home-services)))

   ;; SLiM display manager with NVIDIA Xorg config
   (service slim-service-type
            (slim-configuration
             (auto-login? #f)
             (default-user "rafael")
             (xorg-configuration nvidia-xorg-config)))))

(define einstein-system
  (operating-system
   (inherit (make-desktop-base-os einstein-config
                                  #:extra-packages einstein-extra-packages
                                  #:extra-services einstein-services
                                  ;; Allow Synergy for keyboard/mouse sharing
                                  #:firewall-extra-tcp-ports '(24800)))

   ;; NVIDIA-specific kernel arguments
   (kernel-arguments (gpu-kernel-arguments 'nvidia))

   ;; User configuration (add lp and cgroup to supplementary groups)
   ;; Note: cgroup group now defined in base.scm
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  ;; Include all base groups + lp (printers) + cgroup (containers)
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "realtime" "audio" "video" "lp" "cgroup")))
                 %base-user-accounts))

   ;; Bootloader configuration
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Additional kernel modules for VMD
   (initrd-modules (append '("vmd") %base-initrd-modules))

   ;; File systems
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "be34730a-1ca8-421f-89e7-4a651f90888c"
                                  'btrfs))
                         (type "btrfs"))
                        (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "E174-0557"
                                       'fat32))
                         (type "vfat"))
                        %base-file-systems))))

;;; Apply NVIDIA transformation and export

((nonguix-transformation-nvidia #:configure-xorg? #f) einstein-system)
