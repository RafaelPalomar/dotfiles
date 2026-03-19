(define-module (entelequia system machines lovelace)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers server-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib server-services)
  #:use-module (entelequia systems server)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles server)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services guix)
  #:use-module (gnu services networking)
  #:use-module (gnu system accounts)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (guix gexp)
  #:export (lovelace-os))

(use-service-modules containers networking)

;;; Lovelace system configuration
;;;
;;; Headless home server. Runs self-hosted services via rootless Podman
;;; (oci-service-type with runtime=podman). Each service has a Tailscale sidecar
;;; for per-service tailnet identity and HTTPS access. Secrets managed
;;; by sops-guix — decrypted to /run/secrets/ at boot.
;;;
;;; Hardware: AMD Opteron X3421, 7.5GB RAM, 930GB root (ext4), 1.9TB btrfs RAID.
;;; Boot mode: EFI. Network interface: enp2s0f0.

;;; Machine configuration

(define lovelace-config
  (machine-config
   (hostname "lovelace")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl"))
   (gpu-type #f)
   (machine-type 'server)))

;;; SOPS encrypted secrets file (in git, encrypted)

(define %sops-lovelace
  (local-file "../../sops/lovelace.yaml"))

;;; Home environment — minimal server setup

(define lovelace-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (server-home-packages)))
   (services server-home-services)))

;;; Lovelace-specific services

(define lovelace-services
  (list
   ;; ── Rootless Podman setup (subuid/subgid for rafael) ──────────────────
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subuids (list (subid-range (name "rafael"))))
             (subgids (list (subid-range (name "rafael"))))))

   ;; ── sops-guix: decrypt secrets to /run/secrets/ at boot ───────────────
   ;; GPG key must be pre-deployed to /var/lib/sops before first guix deploy.
   ;; Generate key: gpg --homedir /var/lib/sops --batch --generate-key
   (service sops-secrets-service-type
            (sops-service-configuration
             (sops sops)
             (gnupg-home "/var/lib/sops")
             (secrets
              (list
               ;; PostgreSQL credentials
               (sops-secret (key '("postgresql" "freshrss_password"))
                            (file %sops-lovelace))
               (sops-secret (key '("postgresql" "nextcloud_password"))
                            (file %sops-lovelace))
               (sops-secret (key '("postgresql" "wallabag_password"))
                            (file %sops-lovelace))
               ;; Tailscale auth keys (one per sidecar)
               (sops-secret (key '("tailscale" "freshrss_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "nextcloud_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "wallabag_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "rss_bridge_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "searxng_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "pihole_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "qbt_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "prometheus_authkey"))
                            (file %sops-lovelace))
               (sops-secret (key '("tailscale" "grafana_authkey"))
                            (file %sops-lovelace))
               ;; Mullvad VPN keys
               (sops-secret (key '("mullvad" "pihole_wg_private_key"))
                            (file %sops-lovelace)
                            (permissions #o400))
               (sops-secret (key '("mullvad" "qbt_wg_private_key"))
                            (file %sops-lovelace)
                            (permissions #o400))
               ;; Service credentials
               (sops-secret (key '("pihole" "webpassword"))
                            (file %sops-lovelace))
               (sops-secret (key '("searxng" "secret_key"))
                            (file %sops-lovelace))
               (sops-secret (key '("grafana" "admin_password"))
                            (file %sops-lovelace))
               ;; Borg backup
               (sops-secret (key '("borg" "passphrase"))
                            (file %sops-lovelace))
               (sops-secret (key '("borg" "ssh_private_key"))
                            (file %sops-lovelace)
                            (permissions #o400))))))

   ;; ── Guix Home ─────────────────────────────────────────────────────────
   (service guix-home-service-type
            `(("rafael" ,lovelace-home-env)))))

;;; Lovelace operating system

(define lovelace-system
  (operating-system
   (inherit (make-server-base-os
             lovelace-config
             #:extra-packages
             (specifications->packages lovelace-specific-packages)
             #:extra-services
             (append
              lovelace-services
              postgresql-lovelace-service
              smartd-lovelace-service
              luanti-lovelace-service
              borgmatic-lovelace-service
              lovelace-container-services)
             #:ssh-authorized-keys
             `(("root"   ,(plain-file "monk-access.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1k6qoXg+tPB5tQjDu690RvaICgd8TJYWPCp+U9UJTi rafael@curie"))
               ("rafael" ,(plain-file "monk-access-rafael.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1k6qoXg+tPB5tQjDu690RvaICgd8TJYWPCp+U9UJTi rafael@curie")))
             #:firewall-extra-tcp-ports
             '(53)       ; DNS (Pi-hole)
             #:firewall-extra-udp-ports
             '(53        ; DNS (Pi-hole)
               30000)    ; Luanti game server
             #:enable-ip-forwarding? #t))

   ;; Single user (no desktop, no audio groups)
   (users (cons (user-account
                 (name "rafael")
                 (comment "Rafael")
                 (group "users")
                 (home-directory "/home/rafael")
                 (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                         "cgroup")))
                %base-user-accounts))

   ;; EFI bootloader
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Swap
   (swap-devices (list (swap-space
                        (target (uuid "98cbd6d6-3a65-4fbf-93a5-ac71fd21ffe2")))))

   ;; File systems
   ;; Root: /dev/sdd2 (ext4, 930GB)
   ;; EFI:  /dev/sdd1 (vfat, 512MB)
   ;; Data: /dev/sda+sdb+sdc+sde (btrfs RAID, 1.9TB) — UUID shared across members
   (file-systems (cons* (file-system
                          (mount-point "/")
                          (device (uuid "63c644b6-99ae-4de5-a3ce-3c37b714bdba" 'ext4))
                          (type "ext4"))
                        (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "73A8-A02D" 'fat32))
                          (type "vfat"))
                        (file-system
                          (mount-point "/data")
                          (device (uuid "7d7f761a-3b76-498c-b5e2-b35cef0c32e4" 'btrfs))
                          (type "btrfs")
                          (options "compress=zstd,space_cache=v2"))
                        %base-file-systems))))

;;; Export
(define lovelace-os lovelace-system)
lovelace-os
