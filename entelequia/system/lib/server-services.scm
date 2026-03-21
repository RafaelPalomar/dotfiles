(define-module (entelequia system lib server-services)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services databases)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages luanti)
  #:use-module (gnu packages linux)
  #:use-module (entelequia packages games)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (postgresql-lovelace-service
            smartd-lovelace-service
            luanti-lovelace-service
            borgmatic-lovelace-service
            lovelace-container-services))

;;; Server services for lovelace
;;;
;;; This module defines reusable service configurations for the lovelace server.
;;; Containers use oci-service-type with runtime=podman and user="rafael" for
;;; rootless Podman execution. Secrets are mounted from /run/secrets/ (sops-guix).

;;;
;;; PostgreSQL — shared database for FreshRSS, Nextcloud, Wallabag
;;;

;;; postgresql-lovelace-service: native PostgreSQL with data on /data/postgresql.
;;; Databases and roles must be created manually after first deploy:
;;;   sudo -u postgres psql
;;;   CREATE ROLE freshrss LOGIN PASSWORD '<from /run/secrets/postgresql/freshrss_password>';
;;;   CREATE DATABASE freshrss OWNER freshrss;
;;;   CREATE ROLE nextcloud LOGIN PASSWORD '<from /run/secrets/postgresql/nextcloud_password>';
;;;   CREATE DATABASE nextcloud OWNER nextcloud;
;;;   CREATE ROLE wallabag LOGIN PASSWORD '<from /run/secrets/postgresql/wallabag_password>';
;;;   CREATE DATABASE wallabag OWNER wallabag;
(define postgresql-lovelace-service
  (list
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql-16)
             (data-directory "/data/postgresql")
             (config-file
              (postgresql-config-file
               (extra-config
                '(("listen_addresses" "'localhost'")
                  ("max_connections"  "100")
                  ("shared_buffers"   "'256MB'")
                  ("log_timezone"     "'UTC'")
                  ("timezone"         "'UTC'")))))))))

;;;
;;; smartd — disk health monitoring
;;;

;;; smartd-lovelace-service: shepherd service for smartd disk health monitoring.
(define smartd-lovelace-service
  (list
   (simple-service 'smartd-daemon
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "SMART disk monitoring daemon")
                     (provision '(smartd))
                     (requirement '(file-systems))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append smartmontools "/sbin/smartd")
                                     ;; Foreground, never quit on errors
                                     "-n" "standby"
                                     "-q" "never"
                                     ;; Log to syslog
                                     "-l" "syslog")
                               #:log-file "/var/log/smartd.log"))
                     (stop #~(make-kill-destructor))
                     (respawn? #t))))))

;;;
;;; Luanti — game server
;;;

;;; luanti-lovelace-service: Luanti game server with dedicated system user.
(define luanti-lovelace-service
  (list
   ;; System user for Luanti
   (simple-service 'luanti-user
                   account-service-type
                   (list (user-account
                          (name "luanti")
                          (comment "Luanti game server")
                          (group "nogroup")
                          (system? #t)
                          (home-directory "/var/lib/luanti")
                          (shell (file-append shadow "/sbin/nologin")))))

   ;; Luanti data directories + mineclonia game symlink
   (simple-service 'luanti-dirs
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let ((pw (getpwnam "luanti")))
                            (chown dir (passwd:uid pw) (passwd:gid pw))))
                        '("/data/luanti"
                          "/data/luanti/worlds"
                          "/data/luanti/worlds/mineclonia"
                          "/data/luanti/mods"
                          "/var/lib/luanti"
                          "/var/lib/luanti/games"))
                       ;; Symlink mineclonia game data from system profile
                       (let ((games-dir "/var/lib/luanti/games/mineclonia")
                             (game-src #$(file-append luanti-mineclonia
                                                      "/share/luanti/games/mineclonia")))
                         (unless (file-exists? games-dir)
                           (symlink game-src games-dir)))
                       ;; Symlink mods from system profile
                       (mkdir-p "/var/lib/luanti/mods")
                       (for-each
                        (lambda (mod-src mod-name)
                          (let ((mod-dir (string-append "/var/lib/luanti/mods/" mod-name)))
                            (unless (file-exists? mod-dir)
                              (symlink mod-src mod-dir))))
                        (list #$(file-append luanti-mobs-goblins
                                             "/share/luanti/mods/mobs_goblins"))
                        '("mobs_goblins"))))

   ;; Luanti shepherd service
   (simple-service 'luanti-server
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Luanti game server")
                     (provision '(luanti))
                     (requirement '(file-systems networking))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append luanti-server "/bin/luantiserver")
                                     "--config" "/data/luanti/luanti.conf"
                                     "--world" "/data/luanti/worlds/mineclonia"
                                     "--gameid" "mineclonia"
                                     "--logfile" "/var/log/luanti.log")
                               #:user "luanti"
                               #:group "nogroup"
                               #:directory "/data/luanti"
                               #:environment-variables
                               (list (string-append
                                      "MINETEST_GAME_PATH=/var/lib/luanti/games:"
                                      #$(file-append luanti-server
                                                     "/share/luanti/games"))
                                     "MINETEST_MOD_PATH=/var/lib/luanti/mods")))
                     (stop #~(make-kill-destructor))
                     (respawn? #t))))))

;;;
;;; borgmatic — backup to Hetzner StorageBox
;;;

;;; borgmatic-lovelace-service: daily mcron + on-demand shepherd for borgmatic backup.
(define borgmatic-lovelace-service
  (list
   ;; borgmatic config via etc-service-type
   (simple-service 'borgmatic-config
                   etc-service-type
                   (list `("borgmatic/lovelace.yaml"
                           ,(plain-file "borgmatic-lovelace.yaml"
                                        "# borgmatic configuration for lovelace
# SSH key and passphrase come from sops-guix at /run/secrets/borg/

repositories:
  - path: ssh://u478702-sub1@u478702-sub1.your-storagebox.de:23/./lovelace
    label: hetzner-lovelace

source_directories:
  - /data/freshrss
  - /data/nextcloud
  - /data/wallabag
  - /data/pihole
  - /data/searxng
  - /data/luanti
  - /data/grafana
  # PostgreSQL dump is created by the before_backup hook:
  - /data/postgresql-backup

storage:
  ssh_command: ssh -p 23 -i /run/secrets/borg/ssh_private_key -o StrictHostKeyChecking=accept-new
  encryption_passcommand: cat /run/secrets/borg/passphrase
  encryption: repokey-blake2
  compression: zstd,9
  archive_name_format: lovelace-{now:%Y-%m-%dT%H:%M:%S}

retention:
  keep_daily: 1
  keep_weekly: 1
  keep_monthly: 4

hooks:
  before_backup:
    - mkdir -p /data/postgresql-backup
    - pg_dumpall -U postgres --file=/data/postgresql-backup/full-dump.sql
  after_backup:
    - rm -f /data/postgresql-backup/full-dump.sql
  on_error:
    - echo 'borgmatic failed!' | logger -t borgmatic -p user.err
"))))

   ;; Daily mcron job at 03:30
   (simple-service 'borgmatic-cron
                   mcron-service-type
                   (list
                    #~(job "30 3 * * *"
                           (lambda ()
                             (system* #$(file-append borgmatic "/bin/borgmatic")
                                      "--config" "/etc/borgmatic/lovelace.yaml"
                                      "--verbosity" "1"))
                           "borgmatic-daily")))

   ;; On-demand shepherd one-shot (herd start borgmatic)
   (simple-service 'borgmatic-shepherd
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Run borgmatic backup on demand")
                     (provision '(borgmatic))
                     (requirement '(sops-secrets networking))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append borgmatic "/bin/borgmatic")
                                     "--config" "/etc/borgmatic/lovelace.yaml"
                                     "--verbosity" "1")
                               #:log-file "/var/log/borgmatic.log"))
                     (stop #~(make-kill-destructor))
                     (one-shot? #t)
                     (auto-start? #f))))))

;;;
;;; OCI container helpers — Tailscale sidecar + app pair
;;;
;;; Each service deployment is a pair:
;;;   ts-<name>  : Tailscale sidecar with TS_USERSPACE=true
;;;   <name>     : App container sharing sidecar's network namespace
;;;
;;; Secrets come from /run/secrets/ (sops-guix).
;;; All containers are collected into a single oci-service-type with runtime=podman.
;;;

(define* (make-ts-sidecar name
                           #:key
                           (serve-port 8080)
                           (ts-state-dir (string-append "/data/tailscale/" name)))
  "Return an oci-container-configuration for a Tailscale sidecar.
   NAME is used for the provision name (ts-<name>).
   TS_AUTHKEY is read from /run/secrets/tailscale/<name>_authkey."
  (oci-container-configuration
   (user "rafael")
   (image "tailscale/tailscale:latest")
   (provision (string-append "ts-" name))
   (requirement '(sops-secrets networking))
   (respawn? #t)
   (volumes
    (list (string-append ts-state-dir ":/var/lib/tailscale")
          (string-append "/run/secrets/tailscale/" name "_authkey"
                         ":/run/secrets/ts-authkey:ro")))
   (environment
    (list "TS_USERSPACE=true"
          "TS_STATE_DIR=/var/lib/tailscale"
          "TS_AUTHKEY_FILE=/run/secrets/ts-authkey"
          (string-append "TS_SERVE_PORT=" (number->string serve-port))))
   (extra-arguments '("--cap-add=NET_ADMIN"))))

(define* (make-app-container name image
                              #:key
                              (ts-name name)
                              (volumes '())
                              (environment '())
                              (requirement '())
                              (extra-arguments '()))
  "Return an oci-container-configuration sharing a Tailscale sidecar's network.
   NAME is the provision name.  TS-NAME is the sidecar to share network with."
  (oci-container-configuration
   (user "rafael")
   (image image)
   (provision name)
   (requirement (cons* (string->symbol (string-append "ts-" ts-name))
                       'sops-secrets
                       requirement))
   (respawn? #t)
   (volumes volumes)
   (environment environment)
   (network (string-append "container:ts-" ts-name))
   (extra-arguments extra-arguments)))

;;;
;;; Application container configurations (oci-container-configuration records)
;;;

(define %app-containers
  (list
   ;; ── FreshRSS ──────────────────────────────────────────────────────────
   (make-ts-sidecar "freshrss" #:serve-port 80)
   (make-app-container
    "freshrss" "freshrss/freshrss:latest"
    #:volumes
    (list "/data/freshrss/data:/var/www/FreshRSS/data"
          "/data/freshrss/extensions:/var/www/FreshRSS/extensions"
          "/run/secrets/postgresql/freshrss_password:/run/secrets/db_password:ro")
    #:environment
    (list "CRON_MIN=*/15"
          "TZ=Europe/Oslo"
          "DB_TYPE=pgsql"
          "DB_HOST=host.containers.internal"
          "DB_PORT=5432"
          "DB_BASE=freshrss"
          "DB_USER=freshrss"
          "DB_PASSWORD_FILE=/run/secrets/db_password"))

   ;; ── Nextcloud ─────────────────────────────────────────────────────────
   (make-ts-sidecar "nextcloud" #:serve-port 80)
   (make-app-container
    "nextcloud" "lscr.io/linuxserver/nextcloud:latest"
    #:volumes
    (list "/data/nextcloud/config:/config"
          "/data/nextcloud/data:/data"
          "/run/secrets/postgresql/nextcloud_password:/run/secrets/db_password:ro")
    #:environment
    (list "PUID=1000" "PGID=1000" "TZ=Europe/Oslo"
          "DB_TYPE=pgsql"
          "DB_HOST=host.containers.internal"
          "DB_PORT=5432"
          "DB_NAME=nextcloud"
          "DB_USER=nextcloud"
          "DB_PASSWORD_FILE=/run/secrets/db_password"))

   ;; ── Wallabag ──────────────────────────────────────────────────────────
   (make-ts-sidecar "wallabag" #:serve-port 80)
   (make-app-container
    "wallabag" "wallabag/wallabag:latest"
    #:volumes
    (list "/data/wallabag/data:/var/www/wallabag/data"
          "/data/wallabag/images:/var/www/wallabag/web/assets/images"
          "/run/secrets/postgresql/wallabag_password:/run/secrets/db_password:ro")
    #:environment
    (list "SYMFONY__ENV__DATABASE_DRIVER=pdo_pgsql"
          "SYMFONY__ENV__DATABASE_HOST=host.containers.internal"
          "SYMFONY__ENV__DATABASE_PORT=5432"
          "SYMFONY__ENV__DATABASE_NAME=wallabag"
          "SYMFONY__ENV__DATABASE_USER=wallabag"
          "SYMFONY__ENV__DATABASE_PASSWORD_FILE=/run/secrets/db_password"
          "SYMFONY__ENV__DOMAIN_NAME=https://wallabag"))

   ;; ── RSS-Bridge ────────────────────────────────────────────────────────
   (make-ts-sidecar "rss-bridge" #:serve-port 80)
   (make-app-container
    "rss-bridge" "rssbridge/rss-bridge:latest"
    #:volumes (list "/data/rss-bridge:/app/config"))

   ;; ── SearxNG ───────────────────────────────────────────────────────────
   (make-ts-sidecar "searxng" #:serve-port 8080)
   (make-app-container
    "searxng" "searxng/searxng:latest"
    #:volumes
    (list "/data/searxng:/etc/searxng:rw"
          "/run/secrets/searxng/secret_key:/run/secrets/secret_key:ro")
    #:environment (list "SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml")
    #:extra-arguments
    (list "--cap-drop=ALL" "--cap-add=CHOWN" "--cap-add=SETGID" "--cap-add=SETUID"))))

;;;
;;; VPN-routed containers (Pi-hole + qBittorrent via Gluetun/Mullvad)
;;;

(define %vpn-containers
  (list
   ;; ── Pi-hole ───────────────────────────────────────────────────────────
   ;; Gluetun creates a VPN network namespace; Pi-hole and Tailscale sidecar
   ;; connect to it differently: pihole shares gluetun's netns, ts-pihole
   ;; is separate (proxies to the published port).
   (oci-container-configuration
    (user "rafael")
    (image "qmcgaw/gluetun:latest")
    (provision "gluetun-pihole")
    (requirement '(sops-secrets networking))
    (respawn? #t)
    (volumes
     (list "/data/gluetun-pihole:/gluetun"
           "/run/secrets/mullvad/pihole_wg_private_key:/run/secrets/wg-key:ro"))
    (environment
     (list "VPN_SERVICE_PROVIDER=mullvad"
           "VPN_TYPE=wireguard"
           "WIREGUARD_PRIVATE_KEY_FILE=/run/secrets/wg-key"))
    (ports (list "53:53/tcp" "53:53/udp" "127.0.0.1:8053:8053"))
    (extra-arguments (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun")))

   (oci-container-configuration
    (user "rafael")
    (image "pihole/pihole:latest")
    (provision "pihole")
    (requirement '(gluetun-pihole sops-secrets))
    (respawn? #t)
    (volumes
     (list "/data/pihole/etc:/etc/pihole"
           "/data/pihole/dnsmasq:/etc/dnsmasq.d"
           "/run/secrets/pihole/webpassword:/run/secrets/webpassword:ro"))
    (environment
     (list "WEBPASSWORD_FILE=/run/secrets/webpassword"
           "TZ=Europe/Oslo"
           "DNSMASQ_LISTENING=all"))
    (network "container:gluetun-pihole"))

   (make-ts-sidecar "pihole" #:serve-port 8053
                    #:ts-state-dir "/data/tailscale/pihole")

   ;; ── qBittorrent ───────────────────────────────────────────────────────
   (oci-container-configuration
    (user "rafael")
    (image "qmcgaw/gluetun:latest")
    (provision "gluetun-qbt")
    (requirement '(sops-secrets networking))
    (respawn? #t)
    (volumes
     (list "/data/gluetun-qbt:/gluetun"
           "/run/secrets/mullvad/qbt_wg_private_key:/run/secrets/wg-key:ro"))
    (environment
     (list "VPN_SERVICE_PROVIDER=mullvad"
           "VPN_TYPE=wireguard"
           "WIREGUARD_PRIVATE_KEY_FILE=/run/secrets/wg-key"))
    (ports (list "127.0.0.1:8080:8080"))
    (extra-arguments (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun")))

   (oci-container-configuration
    (user "rafael")
    (image "lscr.io/linuxserver/qbittorrent:latest")
    (provision "qbittorrent")
    (requirement '(gluetun-qbt sops-secrets))
    (respawn? #t)
    (volumes
     (list "/data/qbittorrent/config:/config"
           "/data/qbittorrent/downloads:/downloads"))
    (environment
     (list "PUID=1000" "PGID=1000" "TZ=Europe/Oslo" "WEBUI_PORT=8080"))
    (network "container:gluetun-qbt"))

   (make-ts-sidecar "qbt" #:serve-port 8080
                    #:ts-state-dir "/data/tailscale/qbt")))

;;;
;;; Monitoring containers (Prometheus + Grafana + smartctl-exporter)
;;;

(define %monitoring-containers
  (list
   ;; ── smartctl-exporter ─────────────────────────────────────────────────
   ;; No TS sidecar — scraped internally by Prometheus on the host network.
   (oci-container-configuration
    (image "prometheuscommunity/smartctl-exporter:latest")
    (provision "smartctl-exporter")
    (requirement '(networking))
    (respawn? #t)
    (network "host")
    (extra-arguments (list "--privileged")))

   ;; ── Prometheus ────────────────────────────────────────────────────────
   (make-ts-sidecar "prometheus" #:serve-port 9090)
   (make-app-container
    "prometheus" "prom/prometheus:latest"
    #:volumes
    (list "/data/prometheus:/prometheus"
          "/data/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml:ro")
    #:extra-arguments
    (list "--storage.tsdb.path=/prometheus" "--web.listen-address=:9090"))

   ;; ── Grafana ───────────────────────────────────────────────────────────
   (make-ts-sidecar "grafana" #:serve-port 3000)
   (make-app-container
    "grafana" "grafana/grafana:latest"
    #:volumes
    (list "/data/grafana:/var/lib/grafana"
          "/run/secrets/grafana/admin_password:/run/secrets/grafana-admin-pw:ro")
    #:environment
    (list "GF_SECURITY_ADMIN_PASSWORD__FILE=/run/secrets/grafana-admin-pw"
          "GF_PATHS_DATA=/var/lib/grafana"
          "GF_SERVER_HTTP_PORT=3000"))))

;;;
;;; Single oci-service-type with all containers
;;;

;;; lovelace-container-services: one oci-service-type (runtime=podman) for all containers.
;;; Note: rootless-podman-service-type MUST be in the system services list separately.
;;; It provides: cgroup group creation, subids for rafael, and the shepherd services
;;; (cgroups2-fs-owner, cgroups2-limits, rootless-podman-shared-root-fs) that oci
;;; container services require. The oci-configuration uses the default global user
;;; ("oci-container") to avoid duplicating the "rafael" system account — each container
;;; specifies (user "rafael") directly.
(define lovelace-container-services
  (list
   (service oci-service-type
            (oci-configuration
             (runtime 'podman)
             (containers (append %app-containers
                                 %vpn-containers
                                 %monitoring-containers))))))
