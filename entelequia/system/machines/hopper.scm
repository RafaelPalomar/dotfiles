(define-module (entelequia system machines hopper)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (sops packages sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (guix gexp)
  #:export (hopper-os))

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

;;; Hopper-specific packages
;;
;; intel-microcode: CPU vulnerability mitigations + errata fixes.
;; tlp / powertop: laptop power tooling so tlp-stat / powertop work
;; system-wide (TLP service is enabled via machine-type 'laptop).
;; lm-sensors: provides the `sensors` CLI for thermal/voltage probes.

;; intel-microcode comes via gpu-driver-packages ('intel) in desktop-base;
;; thermald via the (intel, laptop) conditional in the base layer.
(define hopper-extra-packages
  (append
   (specifications->packages '("tlp" "powertop" "lm-sensors"))
   (specifications->packages workstation-packages)))

;;; Home environments for rafael and adrian live in
;;; entelequia/home/machines/hopper-rafael.scm and hopper-adrian.scm
;;; respectively, and are deployed independently per-user via
;;; `guix home reconfigure' (alias `home-reconfigure').

;;; SOPS encrypted secrets file (in git, encrypted). Decrypted at boot by the
;;; Hopper SOPS key in /root/.gnupg (generated on the host).  NOTE: this file
;;; must exist before reconfiguring — create sops/hopper.yaml with the
;;; openrouter/adrian key (mirror sops/alucard.yaml).
(define %sops-hopper
  (local-file "../../../sops/hopper.yaml"))

;;; Hopper-specific services

(define hopper-services
  (list
   ;; sops-guix: decrypt per-machine secrets to /run/secrets/ at boot.
   ;; openrouter/adrian -> /run/secrets/openrouter/adrian (owner adrian, 0400),
   ;; read by the Archimedes launcher (adrian's home service).
   (service sops-secrets-service-type
            (sops-service-configuration
             (sops sops)
             (gnupg-home "/root/.gnupg")
             (secrets
              (list (sops-secret (key '("openrouter" "adrian"))
                                 (file %sops-hopper)
                                 (user "adrian")
                                 (permissions #o400))))))
))

(define hopper-os
  (operating-system
   (inherit (make-desktop-base-os hopper-config
                                  #:extra-packages hopper-extra-packages
                                  #:extra-services hopper-services
                                  ;; adrian: account-only (games, learning)
                                  #:extra-user-accounts
                                  (list (user-account
                                         (name "adrian")
                                         (comment "Adrian")
                                         (group "users")
                                         (home-directory "/home/adrian")
                                         (supplementary-groups
                                          '("netdev" "audio" "video"))))
                                  ;; Laptop power tweaks for the XPS 13 (Kaby Lake R)
                                  ;;   i915.enable_psr=1     Panel Self Refresh
                                  ;;   i915.enable_fbc=1     Frame Buffer Compression
                                  ;;   pcie_aspm=force       Aggressive PCIe ASPM
                                  ;;   mem_sleep_default=deep  deep S3 over s2idle
                                  #:extra-kernel-arguments '("i915.enable_psr=1"
                                                             "i915.enable_fbc=1"
                                                             "pcie_aspm=force"
                                                             "mem_sleep_default=deep")
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file
                                              "hopper-deploy.pub"
                                              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJKoGmzFMaiX/JdtOXJjejf0X7gjG++0qF3uEJWCOrfu hopper-deploy [A]:0x0266C7CE")))))

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
