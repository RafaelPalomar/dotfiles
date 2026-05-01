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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (sops packages sops)
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
  (local-file "../../../sops/lovelace.yaml"))

;;; Home environment — minimal server setup

(define lovelace-home-env
  (home-environment
   (packages (append (base-home-packages)
                     (server-home-packages)))
   (services server-home-services)))

;;; Lovelace-specific services

(define lovelace-services
  (list
   ;; ── rootless Podman setup ─────────────────────────────────────────────
   ;; rootless-podman-service-type MUST be present: it creates the 'cgroup' group,
   ;; configures subids for rafael, and provides the shepherd services
   ;; (cgroups2-fs-owner, cgroups2-limits, rootless-podman-shared-root-fs)
   ;; that oci-service-type containers depend on.
   ;; Note: oci-service-type also extends subids-service-type, which is fine —
   ;; multiple extensions to the same service type are normal in Guix.
   (service rootless-podman-service-type)

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
               ;; #o444: world-readable so rootless Podman containers can read them.
               (sops-secret (key '("postgresql" "freshrss_password"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("postgresql" "nextcloud_password"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("postgresql" "wallabag_password"))
                            (file %sops-lovelace)
                            (permissions #o444))
               ;; Tailscale auth keys (one per sidecar)
               ;; #o444: world-readable so rootless Podman containers can read them.
               ;; In rootless Podman host uid 1000 (rafael) runs as root inside the
               ;; container, but host root (uid 0) is unmapped — so "other" bits apply.
               (sops-secret (key '("tailscale" "freshrss_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "nextcloud_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "wallabag_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "rss_bridge_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "searxng_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "pihole_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "qbt_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "prometheus_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "grafana_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("tailscale" "habitica_authkey"))
                            (file %sops-lovelace)
                            (permissions #o444))
               ;; Habitica server secrets — read by the wrapper entrypoint and
               ;; re-exported as SESSION_SECRET / SESSION_SECRET_KEY env vars.
               (sops-secret (key '("habitica" "session_secret"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("habitica" "session_secret_key"))
                            (file %sops-lovelace)
                            (permissions #o444))
               ;; Mullvad VPN keys — group=users so rootless containers (rafael) can read
               (sops-secret (key '("mullvad" "pihole_wg_private_key"))
                            (file %sops-lovelace)
                            (user "root")
                            (group "users")
                            (permissions #o440))
               (sops-secret (key '("mullvad" "pihole_wg_address"))
                            (file %sops-lovelace)
                            (user "root")
                            (group "users")
                            (permissions #o440))
               (sops-secret (key '("mullvad" "qbt_wg_private_key"))
                            (file %sops-lovelace)
                            (user "root")
                            (group "users")
                            (permissions #o440))
               (sops-secret (key '("mullvad" "qbt_wg_address"))
                            (file %sops-lovelace)
                            (user "root")
                            (group "users")
                            (permissions #o440))
               ;; Service credentials — #o444 for container readability
               (sops-secret (key '("pihole" "webpassword"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("searxng" "secret_key"))
                            (file %sops-lovelace)
                            (permissions #o444))
               (sops-secret (key '("grafana" "admin_password"))
                            (file %sops-lovelace)
                            (permissions #o444))
               ;; Borg backup
               (sops-secret (key '("borg" "passphrase"))
                            (file %sops-lovelace)
                            (permissions #o444))
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
              podman-prune-service
              lovelace-nfs-service
              lovelace-data-dir-service
              nextcloud-proxy-config-service
              postgresql-lovelace-service
              smartd-lovelace-service
              luanti-lovelace-service
              borgmatic-lovelace-service
              lovelace-container-services
              (list habitica-rs-init-service))
             #:ssh-authorized-keys
             `(("root"   ,(plain-file "lovelace-deploy.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJd+gIEzNyO8gp3FnZnvMI/OhKm0/Hkr0UaDKXx38h7V openpgp:0x96CFC574"))
               ("rafael" ,(plain-file "lovelace-deploy-rafael.pub"
                                      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJd+gIEzNyO8gp3FnZnvMI/OhKm0/Hkr0UaDKXx38h7V openpgp:0x96CFC574")))
             #:firewall-extra-tcp-ports
             '(53        ; DNS (Pi-hole)
               2049)     ; NFS server (Edison)
             #:firewall-extra-udp-ports
             '(53        ; DNS (Pi-hole)
               2049      ; NFS server (Edison)
               30000)    ; Luanti game server
             #:enable-ip-forwarding? #t))

   ;; Single user (no desktop, no audio groups)
   ;; oci-container is also defined here to add it to the 'disk' group so that
   ;; the smartctl-exporter container (run as oci-container via rootless Podman)
   ;; can access block devices (/dev/sda–sdf) for SMART health monitoring.
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "cgroup")))
                 (user-account
                  (name "oci-container")
                  (group "users")
                  (system? #t)
                  (comment "OCI services account")
                  (home-directory "/home/oci-container")
                  (shell (file-append shadow "/sbin/nologin"))
                  (supplementary-groups '("cgroup" "disk")))
                 %base-user-accounts))

   ;; EFI bootloader
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl"))))

   ;; Swap
   (swap-devices (list (swap-space
                        (target (uuid "258f095d-9a84-4c83-aade-12922ff0768b")))))

   ;; File systems
   ;; Root: /dev/sda3 (ext4, 927GB)
   ;; EFI:  /dev/sda1 (vfat, 512MB)
   ;; Swap: /dev/sda2 (3.7GB)
   ;; Data: /dev/sdb+sdc+sdd+sde (btrfs RAID, 4×932GB) — UUID shared across members
   (file-systems (cons* (file-system
                          (mount-point "/")
                          (device (uuid "6720085b-9e7b-4985-853c-5956b84edb39" 'ext4))
                          (type "ext4"))
                        (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "73A8-A02D" 'fat32))
                          (type "vfat"))
                        (file-system
                          (mount-point "/data")
                          (device (uuid "5ede0c23-b59f-4f69-830b-27a333356c8d" 'btrfs))
                          (type "btrfs")
                          ;; Explicit device list: btrfs RAID10 needs all 4 members
                          ;; registered with the kernel before open_ctree can succeed.
                          ;; mount-may-fail? #t ensures boot proceeds even if /data
                          ;; isn't ready yet (containers simply won't start).
                          (options "compress=zstd,space_cache=v2,device=/dev/sdb,device=/dev/sdc,device=/dev/sdd,device=/dev/sde")
                          (check? #f)
                          (mount-may-fail? #t))
                        %base-file-systems))))

;;; Export
(define lovelace-os lovelace-system)
lovelace-os
