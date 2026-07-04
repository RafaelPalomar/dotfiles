(define-module (entelequia system machines einstein)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia packages latex)
  #:use-module (entelequia system lib common-services)
  #:use-module (entelequia system lib pam-gnupg)
  #:use-module (entelequia system lib chromium-policy)
  #:use-module (entelequia system machines datalocker-udev-rules)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (nonguix transformations)
  #:export (einstein-os))

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

;;; Einstein-specific packages

(define einstein-extra-packages
  (append
   (specifications->packages einstein-specific-packages)
   (specifications->packages base-latex-packages)
   (list font-sciflycore-sans latex-nfr)))

;;; Einstein system definition

;; Define einstein-specific services
(define einstein-services
  (list
   ;; Game controller udev rules (PS4, PS5, Xbox, etc.)
   gamepad-udev-rules-service

   ;; Allow non-bonded Bluetooth HID devices (PS5 DualSense, etc.)
   bluetooth-input-config-service

   ;; DataLocker Sentry ONE auto-unlock udev rule
   datalocker-udev-rules-service

   ;; Home environment for rafael lives in
   ;; entelequia/home/machines/einstein-rafael.scm and is deployed
   ;; independently via `guix home reconfigure' (alias `home-reconfigure').

   ;; pam-gnupg: SLiM login password → gpg-agent passphrase cache.
   ;; Eliminates pinentry prompts for keygrips listed in ~/.pam-gnupg.
   ;; Requires the GPG passphrase to equal the login password.
   (service pam-gnupg-service-type)

   ;; Chromium managed policy: SearXNG default search + Bitwarden forcelist.
   ;; Per-profile launchers come from the home environment.
   chromium-policy-service))

(define einstein-system
  (operating-system
   (inherit (make-desktop-base-os einstein-config
                                  #:extra-packages einstein-extra-packages
                                  #:extra-services einstein-services
                                  ;; lp (printers) + dialout on top of the
                                  ;; default cgroup (containers)
                                  #:extra-user-groups '("lp" "cgroup" "dialout")
                                  ;; Allow Synergy for keyboard/mouse sharing
                                  #:firewall-extra-tcp-ports '(24800)))

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

(define einstein-os
  ((nonguix-transformation-nvidia #:configure-xorg? #f) einstein-system))

einstein-os
