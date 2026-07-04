(define-module (entelequia system machines alucard)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (nonguix transformations)
  #:use-module (sops packages sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (guix gexp)
  #:export (alucard-os))

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

;;; Alucard-specific packages

(define alucard-extra-packages
  (specifications->packages alucard-specific-packages))

;;; Home environments for rafael and leandro live in
;;; entelequia/home/machines/alucard-rafael.scm and alucard-leandro.scm
;;; respectively, and are deployed independently per-user via
;;; `guix home reconfigure' (alias `home-reconfigure').

;;; SOPS encrypted secrets file (in git, encrypted). Decrypted at boot by the
;;; Alucard SOPS key in /root/.gnupg (generated on the host).
(define %sops-alucard
  (local-file "../../../sops/alucard.yaml"))

;;; Alucard-specific services

(define alucard-services
  (list
   ;; sops-guix: decrypt per-machine secrets to /run/secrets/ at boot.
   ;; openrouter/leandro -> /run/secrets/openrouter/leandro (owner leandro, 0400),
   ;; read by the Archimedes launcher (leandro's home service).
   (service sops-secrets-service-type
            (sops-service-configuration
             (sops sops)
             (gnupg-home "/root/.gnupg")
             (secrets
              (list (sops-secret (key '("openrouter" "leandro"))
                                 (file %sops-alucard)
                                 (user "leandro")
                                 (permissions #o400))))))
))

(define alucard-system
  (operating-system
   (inherit (make-desktop-base-os alucard-config
                                  #:extra-packages alucard-extra-packages
                                  #:extra-services alucard-services
                                  ;; Second user of the shared desktop.
                                  #:extra-user-accounts
                                  (list (user-account
                                         (name "leandro")
                                         (comment "Leandro")
                                         (group "users")
                                         (home-directory "/home/leandro")
                                         (supplementary-groups
                                          '("wheel" "netdev" "audio" "video"))))
                                  #:firewall-extra-tcp-ports '(4549)
                                  #:firewall-extra-udp-ports '(4549 4171 4175 4179)
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file "monk-access.pub"
                                                         "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1k6qoXg+tPB5tQjDu690RvaICgd8TJYWPCp+U9UJTi rafael@curie"))
                                    ("rafael" ,(plain-file "rafael-curie.pub"
                                                           "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCMfBOPdeKIKGekKgBOxJSozJ/jCrnZas657mege/d7VuhXQ1nSvd4en2PjrYTNN0hRgUQ4ccrJpPpKOrLdS5UB3YyZbrTjdQFHjDeEhkaO9dyphfWL0OeVVj1VC4j0/PlIhBqOOdfgC1+Y+z2+6P8xFILWolH7d4yYNCKANz0sUVorPVRYc388S7PSiBZOf4ZVcdEFql6uqDiMVtWlkXtq/4DcXMDtTudvQvjh1BYAAzAM5TEoYwXL/LHCed010FELX96KdqTZXuBKtEdjW7WX85IYWhw05vaSYNyML0DA6trvD7qAOmQ5SDXXot/Vkyf8aX36Xwhu2yoVTKBxdVvklkSZSrvTigpvlPFphFRkF2j6B6A8uIalKLoHZecE+xyCfq+0aUaHz6/KDw2N6SkhFg3N4/f5HjlA2j00wLILj6/htI57TNGbffls/Ln9gXwuyq15v4+sIAYyY1LZyjA4WsB/AtO9IZjusJjkQYuu8Zg6SxSkFMaJ3mmNk6rNwhjyVPXbmpBg97+6CrApwQbF4As/h7dcQTbeTIbZdVJbv7TWxRHfqmaGZYzWKNkt+Njd/VmlLnY29D4DJ3zmC/NkXymTeOggt/YMGFr2UHFSYYtjtd8y/0z5bu0tRNXNt2gE0glQhozTuZMSAD4uzzXOC2YnJXqsvhZTPOH0iKJuCQ== rafael@curie")))))

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
