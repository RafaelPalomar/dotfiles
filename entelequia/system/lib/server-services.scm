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
            lovelace-data-dir-service
            lovelace-container-services))

;;; Server services for lovelace
;;;
;;; This module defines reusable service configurations for the lovelace server.
;;; Containers use oci-service-type with runtime=podman and user="rafael" for
;;; rootless Podman execution. Secrets are mounted from /run/secrets/ (sops-guix).

;;;
;;; /data directory structure — created at activation time
;;;

;;; lovelace-data-dir-service: creates all required /data subdirectories at boot.
;;; Must run after file-system-/data is mounted. Idempotent (mkdir -p).
(define lovelace-data-dir-service
  (list
   (simple-service 'lovelace-data-dirs
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let* ((pw  (getpwnam "rafael"))
                                 (uid (passwd:uid pw))
                                 (gid (passwd:gid pw)))
                            (chown dir uid gid)))
                        '("/data/tailscale/freshrss"
                          "/data/tailscale/nextcloud"
                          "/data/tailscale/wallabag"
                          "/data/tailscale/rss-bridge"
                          "/data/tailscale/searxng"
                          "/data/tailscale/pihole"
                          "/data/tailscale/qbt"
                          "/data/tailscale/prometheus"
                          "/data/tailscale/grafana"
                          "/data/freshrss"
                          "/data/nextcloud"
                          "/data/wallabag"
                          "/data/rss-bridge"
                          "/data/searxng"
                          "/data/pihole"
                          "/data/qbittorrent"
                          "/data/gluetun-pihole"
                          "/data/gluetun-qbt"
                          "/data/prometheus"
                          "/data/grafana"
                          "/data/borg"))))))

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
   ;; Create /data/postgresql with postgres:postgres ownership before the
   ;; postgresql shepherd service starts. The standard postgresql-service-type
   ;; activation expects the data directory to already exist when the data
   ;; directory is on a separately mounted volume like /data (btrfs).
   (simple-service 'postgresql-data-dir
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (let* ((dir "/data/postgresql")
                              (pw  (getpwnam "postgres"))
                              (uid (passwd:uid pw))
                              (gid (passwd:gid pw)))
                         (mkdir-p dir)
                         (chown dir uid gid)
                         (chmod dir #o700))))
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql-16)
             (data-directory "/data/postgresql")
             (config-file
              (postgresql-config-file
               ;; listen on all interfaces so rootless Podman containers
               ;; (pasta network, same IP as host) can connect via the LAN IP.
               ;; pg_hba.conf restricts who can authenticate.
               (hba-file
                (plain-file "pg_hba.conf"
                            "\
# TYPE  DATABASE        USER            ADDRESS          METHOD
# local connections via Unix socket
local   all             postgres                         peer
local   all             all                              peer
# host connections from localhost
host    all             all             127.0.0.1/32     md5
host    all             all             ::1/128          md5
# host connections from LAN (for rootless Podman containers via pasta)
host    all             all             192.168.88.0/24  md5
"))
               (extra-config
                '(("listen_addresses" "*")
                  ("max_connections"  "100")
                  ("shared_buffers"   "256MB")
                  ("log_timezone"     "UTC")
                  ("timezone"         "UTC")))))))))

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
                                     "--no-fork"   ; shepherd manages the process
                                     "-q" "never") ; never quit on errors
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
                                     "--logfile" "/data/luanti/luanti.log")
                               #:user "luanti"
                               #:group "nogroup"
                               #:directory "/data/luanti"
                               #:environment-variables
                               (list "HOME=/var/lib/luanti"
                                     (string-append
                                      "LUANTI_GAME_PATH=/var/lib/luanti/games:"
                                      #$(file-append luanti-server
                                                     "/share/luanti/games"))
                                     "LUANTI_MOD_PATH=/var/lib/luanti/mods")))
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
                           (ts-state-dir (string-append "/data/tailscale/" name))
                           ;; Secret file name defaults to NAME with hyphens→underscores.
                           ;; Override when the sops key uses different naming.
                           (secret-name (string-map (lambda (c) (if (char=? c #\-) #\_ c)) name))
                           ;; Optional published ports (list of "host:container" strings).
                           ;; Used to expose a container port to 192.168.88.46
                           ;; so sibling containers can reach it.
                           (ports '()))
  "Return an oci-container-configuration for a Tailscale sidecar.
   NAME is used for the provision name (ts-<name>).
   TS_AUTHKEY is read from /run/secrets/tailscale/<secret-name>_authkey."
  (oci-container-configuration
   (user "rafael")
   (image "tailscale/tailscale:latest")
   (provision (string-append "ts-" name))
   (requirement '(sops-secrets networking))
   (respawn? #t)
   (ports ports)
   (volumes
    (list (string-append ts-state-dir ":/var/lib/tailscale")
          (string-append "/run/secrets/tailscale/" secret-name "_authkey"
                         ":/run/secrets/ts-authkey:ro")))
   (environment
    (list "TS_USERSPACE=true"
          "TS_STATE_DIR=/var/lib/tailscale"
          ;; TS_AUTHKEY_FILE is read by the entrypoint wrapper below and
          ;; re-exported as TS_AUTHKEY, which containerboot understands.
          "TS_AUTHKEY_FILE=/run/secrets/ts-authkey"
          (string-append "TS_SERVE_PORT=" (number->string serve-port))
          ;; Register with a clean hostname (no "ts-" prefix — that's only for
          ;; shepherd service namespacing, not the Tailscale node name).
          (string-append "TS_HOSTNAME=" name)))
   (extra-arguments '("--cap-add=NET_ADMIN"))
   ;; Wrapper: read TS_AUTHKEY_FILE and export as TS_AUTHKEY before
   ;; execing containerboot.  Older image versions don't support
   ;; TS_AUTHKEY_FILE natively; this shell wrapper bridges the gap.
   (entrypoint "/bin/sh")
   (command (list "-c"
                  "export TS_AUTHKEY=$(cat \"$TS_AUTHKEY_FILE\"); exec /usr/local/bin/containerboot"))))

(define* (make-app-container name image
                              #:key
                              (ts-name name)
                              (volumes '())
                              (environment '())
                              (requirement '())
                              (extra-arguments '())
                              (entrypoint #f)
                              (command '()))
  "Return an oci-container-configuration sharing a Tailscale sidecar's network.
   NAME is the provision name.  TS-NAME is the sidecar to share network with.
   COMMAND overrides the image CMD (args passed to the container entrypoint).
   ENTRYPOINT overrides the image ENTRYPOINT when non-#f."
  (let ((base (oci-container-configuration
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
               (extra-arguments extra-arguments)
               (command command))))
    (if entrypoint
        (oci-container-configuration
         (inherit base)
         (entrypoint entrypoint))
        base)))

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
          "DB_HOST=192.168.88.46"
          "DB_PORT=5432"
          "DB_BASE=freshrss"
          "DB_USER=freshrss")
    ;; FreshRSS needs DB_PASSWORD as a plain env var (no native _FILE support).
    ;; Read the secret file and exec the real entrypoint.
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export DB_PASSWORD=$(cat /run/secrets/db_password); exec /entrypoint.sh"))

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
          "DB_HOST=192.168.88.46"
          "DB_PORT=5432"
          "DB_NAME=nextcloud"
          "DB_USER=nextcloud")
    ;; LinuxServer.io image uses /init (s6-overlay) as entrypoint; DB_PASSWORD_FILE
    ;; is not natively supported, so read the file and export the plain var.
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export DB_PASSWORD=$(cat /run/secrets/db_password); exec /init"))

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
          "SYMFONY__ENV__DATABASE_HOST=192.168.88.46"
          "SYMFONY__ENV__DATABASE_PORT=5432"
          "SYMFONY__ENV__DATABASE_NAME=wallabag"
          "SYMFONY__ENV__DATABASE_USER=wallabag"
          "SYMFONY__ENV__DOMAIN_NAME=https://wallabag")
    ;; Wrapper: read DB password file and export as the plain env var, then start
    ;; wallabag.  The entrypoint script requires "wallabag" as its first arg to
    ;; start the web server; without it exec "$@" exits immediately.
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export SYMFONY__ENV__DATABASE_PASSWORD=$(cat /run/secrets/db_password); exec /entrypoint.sh wallabag"))

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
           "VPN_TYPE=wireguard"))
    ;; Gluetun does not support WIREGUARD_PRIVATE_KEY_FILE; read key from file.
    (entrypoint "/bin/sh")
    (command (list "-c"
                   "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); exec /gluetun"))
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
           "VPN_TYPE=wireguard"))
    ;; Gluetun does not support WIREGUARD_PRIVATE_KEY_FILE; read key from file.
    (entrypoint "/bin/sh")
    (command (list "-c"
                   "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); exec /gluetun"))
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

;;; Prometheus scrape config.
;;;
;;; Targets use 192.168.88.46 so prometheus (inside ts-prometheus's
;;; network namespace) can reach natively-running exporters on the host.
(define %prometheus-config
  (plain-file "prometheus.yml"
              "global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: prometheus
    static_configs:
      - targets: ['localhost:9090']

  - job_name: node-exporter
    static_configs:
      - targets: ['localhost:9100']

  - job_name: smartctl-exporter
    static_configs:
      - targets: ['localhost:9633']
"))

;;; Grafana datasource provisioning.
;;;
;;; Points at ts-prometheus's published port 9090 (192.168.88.46
;;; is accessible from ts-grafana's pasta network namespace).
(define %grafana-prometheus-datasource
  (plain-file "prometheus-datasource.yaml"
              "apiVersion: 1
datasources:
  - name: Prometheus
    type: prometheus
    url: http://192.168.88.46:9090
    isDefault: true
    access: proxy
    editable: false
"))

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
   ;; Prometheus runs with host networking so it can scrape native services
   ;; (node-exporter :9100, smartctl-exporter :9633) on the host directly.
   ;; ts-prometheus is NOT used for prometheus itself — instead prometheus
   ;; listens on the host at :9090, and grafana reaches it at 192.168.88.46:9090.
   ;; ts-prometheus is kept as a no-op sidecar for potential future use.
   (make-ts-sidecar "prometheus" #:serve-port 80)
   (oci-container-configuration
    (user "rafael")
    (image "prom/prometheus:latest")
    (provision "prometheus")
    (requirement '(sops-secrets networking cgroups2-fs-owner cgroups2-limits
                   rootless-podman-shared-root-fs user-processes))
    (respawn? #t)
    (network "host")
    (volumes
     (list "/data/prometheus:/prometheus"
           (cons %prometheus-config "/etc/prometheus/prometheus.yml:ro")))
    ;; Run as container root (= host rafael uid 1000) so it can write to
    ;; /data/prometheus, which is owned by rafael.
    (extra-arguments '("--user=0"))
    (command
     (list "--config.file=/etc/prometheus/prometheus.yml"
           "--storage.tsdb.path=/prometheus"
           "--web.listen-address=:9090")))

   ;; ── Grafana ───────────────────────────────────────────────────────────
   ;; Provisioned datasource points prometheus at 192.168.88.46:9090
   ;; (the port published by ts-prometheus above).
   (make-ts-sidecar "grafana" #:serve-port 80)
   (make-app-container
    "grafana" "grafana/grafana:latest"
    #:volumes
    (list "/data/grafana:/var/lib/grafana"
          "/run/secrets/grafana/admin_password:/run/secrets/grafana-admin-pw:ro"
          (cons %grafana-prometheus-datasource
                "/etc/grafana/provisioning/datasources/prometheus.yaml:ro"))
    #:environment
    (list "GF_SECURITY_ADMIN_PASSWORD__FILE=/run/secrets/grafana-admin-pw"
          "GF_PATHS_DATA=/var/lib/grafana"
          "GF_SERVER_HTTP_PORT=80"))))

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
