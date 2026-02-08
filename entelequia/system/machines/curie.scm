(define-module (entelequia system machines curie)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia systems desktop)  ; For desktop-home-services
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (xlibre))

(use-service-modules xorg)

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

(define amd-xlibre-config
  (xlibre-configuration
   (modules (list xlibre-video-amdgpu xlibre-input-libinput))
   (drivers '("modesetting"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))))

;;; Curie-specific packages

(define curie-extra-packages
  (append
   (specifications->packages amd-specific-packages)
   (specifications->packages curie-specific-packages)))

;;; Curie system definition

;; Define curie-specific services
(define curie-services
  (list
   ;; Guix Home configuration
   (guix-home-config
    curie-config
    (home-environment
     (services desktop-home-services)))

   ;; SLiM display manager with AMD Xlibre config
   (service slim-service-type
            (slim-configuration
             (auto-login? #f)
             (default-user "rafael")
             (xorg-configuration amd-xlibre-config)))))

(define curie-system
  (operating-system
   (inherit (make-desktop-base-os curie-config
                                  #:extra-packages curie-extra-packages
                                  #:extra-services curie-services))

   ;; AMD-specific kernel arguments (amd_pstate, network interface naming)
   (kernel-arguments (gpu-kernel-arguments 'amd
                                           #:extra-args '("net.ifnames=0"
                                                          "biosdevname=0")))

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
