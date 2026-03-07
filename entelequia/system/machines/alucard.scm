(define-module (entelequia system machines alucard)
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
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services containers)
  #:use-module (gnu system accounts)
  #:use-module (gnu services guix)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (nonguix transformations)
  #:use-module (xlibre)
  #:export (alucard-os))

(use-service-modules xorg containers)

;;; Alucard system configuration
;;;
;;; Shared desktop system with NVIDIA GPU. Inherits from desktop-base.
;;; Both users (rafael, leandro) run bspwm via .xsession from dotfiles.
;;; SLiM display manager — sessions determined by each user's ~/.xsession.

;;; Machine configuration

(define alucard-config
  (machine-config
   (hostname "alucard")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl"))
   (gpu-type 'nvidia)
   (machine-type 'desktop)))

;;; NVIDIA Xorg configuration

(define nvidia-xorg-config
  (xlibre-configuration
   (modules (list nvidia-driver xlibre-input-libinput))
   (drivers '("nvidia"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

;;; Alucard-specific packages

(define alucard-extra-packages
  (append
   (specifications->packages nvidia-specific-packages)
   (specifications->packages alucard-specific-packages)))

;;; Home environments

;; Rafael's home: full bspwm desktop setup (same as einstein/curie)
(define rafael-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (development-home-packages)
                     email-home-packages
                     documentation-home-packages))
   (services desktop-home-services)))

;; Leandro's home: same bspwm desktop setup as rafael
(define leandro-home-env
  (home-environment
   (packages (base-home-packages))
   (services desktop-home-services)))

;;; Alucard-specific services

(define alucard-services
  (list
   ;; Rootless podman for containerization (rafael)
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subuids (list (subid-range (name "rafael"))))
             (subgids (list (subid-range (name "rafael"))))))

   ;; Guix Home for both users
   (service guix-home-service-type
            `(("rafael" ,rafael-home-env)
              ("leandro" ,leandro-home-env)))

   ;; SLiM display manager with NVIDIA xorg (proven to work with xlibre + NVIDIA)
   ;; Both rafael and leandro use bspwm via .xsession from dotfiles
   (service slim-service-type
            (slim-configuration
             (auto-login? #f)
             (default-user "rafael")
             (xorg-configuration nvidia-xorg-config)))))

(define alucard-system
  (operating-system
   (inherit (make-desktop-base-os alucard-config
                                  #:extra-packages alucard-extra-packages
                                  #:extra-services alucard-services
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file "monk-access.pub"
                                                         "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1k6qoXg+tPB5tQjDu690RvaICgd8TJYWPCp+U9UJTi rafael@curie")))))

   ;; NVIDIA kernel arguments
   (kernel-arguments (gpu-kernel-arguments 'nvidia))

   ;; Both users (cgroup group for rafael's containers)
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "realtime" "audio" "video" "cgroup")))
                 (user-account
                  (name "leandro")
                  (comment "Leandro")
                  (group "users")
                  (home-directory "/home/leandro")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))

   ;; Bootloader
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Swap
   (swap-devices (list (swap-space
                        (target (uuid
                                 "58922844-3a00-461d-be53-2c13db2eacbf")))))

   ;; File systems (UUIDs from installer)
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "7A3E-392A" 'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid
                                  "fb977e55-9372-4f37-9637-686428fae36a"
                                  'ext4))
                         (type "ext4"))
                        %base-file-systems))))

;;; Apply NVIDIA transformation and export

(define alucard-os
  ((nonguix-transformation-nvidia #:configure-xorg? #f) alucard-system))

alucard-os
