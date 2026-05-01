(define-module (entelequia system lib server-services)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services databases)
  #:use-module (gnu services mcron)
  #:use-module (gnu services nfs)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages containers)
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
            nextcloud-proxy-config-service
            lovelace-container-services
            lovelace-nfs-service
            podman-prune-service
            make-ts-sidecar
            make-ts-ready-service
            make-app-container
            %habitica-commit
            habitica-rs-init-service))

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
                        '(;; Media share (exported to Edison via NFS)
                          "/data/media"
                          "/data/media/videos"
                          "/data/media/audiobooks"))
                       ;; Music and rips need world-write so the ARM container on Edison
                       ;; can deposit files. NFS uid mapping is numeric-only (no idmapd
                       ;; name resolution); Edison's rootless Podman maps container uid 0
                       ;; to host uid 1001 (rafael on Edison) which Lovelace sees as an
                       ;; unmapped uid. Mode 1777 (sticky + world-write) allows any process
                       ;; to create files while preventing others from deleting them.
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let* ((pw  (getpwnam "rafael"))
                                 (uid (passwd:uid pw))
                                 (gid (passwd:gid pw)))
                            (chown dir uid gid)
                            (chmod dir #o1777)))
                        '("/data/media/music"
                          "/data/media/rips"
                          "/data/media/movies"
                          "/data/media/tv"))
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
                          "/data/tailscale/searxng-kids"
                          "/data/tailscale/pihole"
                          "/data/tailscale/qbt"
                          "/data/tailscale/prometheus"
                          "/data/tailscale/grafana"
                          "/data/tailscale/habitica"
                          "/data/freshrss"
                          "/data/nextcloud"
                          "/data/wallabag"
                          "/data/rss-bridge"
                          "/data/searxng"
                          "/data/searxng-kids"
                          "/data/pihole"
                          "/data/qbittorrent"
                          "/data/gluetun-pihole"
                          "/data/gluetun-qbt"
                          "/data/prometheus"
                          "/data/grafana"
                          "/data/habitica"
                          "/data/habitica/db"
                          ;; /data/nextcloud/config: non-recursive chown to rafael so
                          ;; container root (= host rafael in rootless Podman) can write.
                          ;; Subdirs are owned by container abc (host uid 232071); do NOT
                          ;; chown recursively or the web server loses access.
                          ;; /data/nextcloud/data is NOT listed: container init owns it.
                          "/data/nextcloud/config"
                          "/data/borg"))))))

;;;
;;; NFS server — export /data/media to Edison (192.168.88.14)
;;;

;;; lovelace-nfs-service: export /data/media over NFS to the LAN.
;;; Edison mounts it as /media. Uses TCP-only NFSv4; port 2049 must be
;;; open in the firewall (firewall-extra-tcp-ports in lovelace.scm).
(define lovelace-nfs-service
  (list
   (service nfs-service-type
            (nfs-configuration
             ;; TCP only (nfsd-udp? defaults to #f)
             (exports
              ;; Each inner list is joined with spaces → one /etc/exports line.
              '(("/data/media"
                 "192.168.88.0/24(rw,sync,no_subtree_check,no_root_squash)")))))))

;;; nextcloud-proxy-config-service: write a declarative config drop-in for
;;; Nextcloud covering trusted domains, trusted proxies, and URL overrides.
;;; The linuxserver/nextcloud image merges all *.config.php files from
;;; /config/www/nextcloud/config/ into the running configuration.
;;; Always overwritten on deploy so changes here take effect immediately.
(define nextcloud-proxy-config-service
  (list
   (simple-service 'nextcloud-proxy-config
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       ;; Container abc user (PUID=1000) maps to host uid 232071
                       ;; via rootless Podman subuid remap. The directory and file
                       ;; must be owned by abc so Nextcloud's PHP runtime can write
                       ;; into config/ (e.g. for trusted-domain updates, upgrades).
                       (let* ((conf-dir "/data/nextcloud/config/www/nextcloud/config")
                              (conf-file (string-append conf-dir "/proxy.config.php"))
                              (abc-uid 232071)
                              (abc-gid 232071))
                         (mkdir-p conf-dir)
                         (chown conf-dir abc-uid abc-gid)
                         (call-with-output-file conf-file
                           (lambda (port)
                             (display "<?php\n$CONFIG = array (\n" port)
                             (display "  'trusted_domains' => array('localhost', 'nextcloud.drake-karat.ts.net'),\n" port)
                             (display "  'trusted_proxies' => array('127.0.0.1', '::1', '192.168.88.46'),\n" port)
                             (display "  'forwarded_for_headers' => array('HTTP_X_FORWARDED_FOR'),\n" port)
                             (display "  'overwriteprotocol' => 'https',\n" port)
                             (display "  'overwrite.cli.url' => 'https://nextcloud.drake-karat.ts.net',\n" port)
                             (display ");\n" port)))
                         (chown conf-file abc-uid abc-gid)
                         (chmod conf-file #o644))))))

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
    - mkdir -p /var/lib/node-exporter/textfile
    - pg_dumpall -U postgres --file=/data/postgresql-backup/full-dump.sql
  after_backup:
    - rm -f /data/postgresql-backup/full-dump.sql
    - echo borgmatic_last_success_timestamp_seconds $(date +%s) > /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - echo borgmatic_last_error 0 >> /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - borgmatic info --json --config /etc/borgmatic/lovelace.yaml > /tmp/borg-info.json
    - echo borgmatic_repository_unique_csize_bytes $(jq -r '.[0].cache.stats.unique_csize' /tmp/borg-info.json) >> /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - echo borgmatic_repository_total_size_bytes $(jq -r '.[0].cache.stats.total_size' /tmp/borg-info.json) >> /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - rm -f /tmp/borg-info.json
    - mv /var/lib/node-exporter/textfile/borgmatic.prom.tmp /var/lib/node-exporter/textfile/borgmatic.prom
  on_error:
    - echo 'borgmatic failed!' | logger -t borgmatic -p user.err
    - echo borgmatic_last_error 1 > /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - echo borgmatic_last_error_timestamp_seconds $(date +%s) >> /var/lib/node-exporter/textfile/borgmatic.prom.tmp
    - mv /var/lib/node-exporter/textfile/borgmatic.prom.tmp /var/lib/node-exporter/textfile/borgmatic.prom
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

;;;
;;; podman-prune-service: remove stale containers at boot before OCI services start
;;;
;;; At boot, container records from the previous run persist in podman's database even
;;; though the processes are gone.  When a sidecar tries to replace itself with --replace,
;;; podman refuses if a dependent app container is still registered.
;;;
;;; `podman container prune -f` only removes stopped containers but fails when a container
;;; has dependents (e.g. ts-* sidecars using --network container:<name> depend on the app
;;; container).  Using `podman rm -af` removes ALL containers regardless of state or
;;; dependency order, which is safe at boot before any container services start.
;;;
;;; Also creates /run/user/<uid> for the rootless podman user, since elogind only creates
;;; it on interactive login (not available on headless servers at boot).
;;;
;;; All OCI containers require this service via make-ts-sidecar / make-app-container.
;;;

(define %podman-prune-script
  (program-file "podman-prune"
    #~(begin
        (let* ((pw   (getpwnam "rafael"))
               (uid  (passwd:uid pw))
               (gid  (passwd:gid pw))
               (ruid (string-append "/run/user/" (number->string uid))))
          ;; Create /run/user/<uid> if missing (elogind won't do it for headless boot)
          (unless (file-exists? ruid)
            (mkdir ruid)
            (chown ruid uid gid)
            (chmod ruid #o700))
          (setenv "XDG_RUNTIME_DIR" ruid)
          (setenv "HOME" (passwd:dir pw))
          ;; Prepend /run/setuid-programs so podman finds the setuid newuidmap
          ;; and newgidmap wrappers.  Without this, podman uses the non-setuid
          ;; copies from /run/current-system/profile/bin and `rm -af` fails for
          ;; containers that were running at reboot time.
          (setenv "PATH"
                  (string-append "/run/setuid-programs:"
                                 (or (getenv "PATH") "")))
          (setgid gid)
          (setuid uid)
          ;; Remove ALL containers at boot before any container service starts.
          ;; This service is one-shot: shepherd waits for this process to exit
          ;; before marking it "started" and allowing dependent services to begin.
          ;; That serialisation guarantees cleanup is complete before any
          ;; `podman run --replace` is attempted by the ts-* sidecars.
          ;;
          ;; A one-shot service in "started" state continues to satisfy the
          ;; requirement of dependent services on respawn, so container services
          ;; can restart without re-triggering this cleanup.
          ;;
          ;; We ignore the exit code of rm -af: it may log newuidmap warnings
          ;; for containers that were running at reboot time, but it still removes
          ;; their entries from podman's storage, which is all that is needed.
          ;;
          ;; Use execlp (not system*) so this guile process is replaced by podman
          ;; directly.  Shepherd then holds the podman PID, and SIGTERM/SIGKILL
          ;; from make-kill-destructor reach it without orphaning a child process.
          ;; Without this, system* forks a child; shepherd kills the guile parent
          ;; but the podman child becomes an orphan and keeps running — causing
          ;; hundreds of stuck `podman rm -af` processes after repeated deploys.
          ;;
          ;; coreutils timeout wraps podman so a hung rm -af cannot block the
          ;; service forever (120 s is generous for removing a handful of containers).
          (execlp #$(file-append coreutils "/bin/timeout")
                  "timeout" "120"
                  #$(file-append podman "/bin/podman") "rm" "-af")))))

(define podman-prune-service
  (list
   (simple-service 'podman-prune
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (provision '(podman-prune))
                     (requirement '(rootless-podman-shared-root-fs user-processes))
                     (one-shot? #t)
                     ;; CRITICAL: respawn? defaults to #t in shepherd-service.
                     ;; A one-shot transitions to "stopped" after it exits, and
                     ;; with respawn? #t shepherd respawns it every few seconds —
                     ;; each respawn runs `podman rm -af`, killing every running
                     ;; container.  Lock respawn off so the cleanup happens once
                     ;; per boot.  Previously fixed in 8c34e57 by running a sleep
                     ;; loop instead, then accidentally undone in 3b02e7e.
                     (respawn? #f)
                     (start #~(make-forkexec-constructor
                               (list #$%podman-prune-script)
                               #:log-file "/var/log/podman-prune.log"))
                     (documentation "Remove all containers from previous boot, then exit."))))))

(define* (make-ts-sidecar name
                           #:key
                           (serve-port 8080)
                           ;; The host address used as the proxy backend.
                           ;; TS_USERSPACE=true uses gVisor netstack, which cannot route
                           ;; to 127.0.0.1 (virtual loopback, unreachable by kernel procs).
                           ;; App containers share this sidecar's pasta network namespace
                           ;; and bind to 0.0.0.0, so they're reachable at the host LAN IP.
                           (backend-host "192.168.88.46")
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
   TS_AUTHKEY is read from /run/secrets/tailscale/<secret-name>_authkey.
   Tailscale serve is configured via TS_SERVE_CONFIG with BACKEND-HOST:SERVE-PORT
   as the proxy backend, bypassing the TS_USERSPACE netstack loopback limitation."
  (let* ((backend-url
          (string-append "http://" backend-host ":" (number->string serve-port)))
         (serve-config-content
          (string-append
           "{\n"
           "  \"TCP\": {\n"
           "    \"443\": {\n"
           "      \"HTTPS\": true\n"
           "    },\n"
           "    \"80\": {\n"
           "      \"HTTPS\": false\n"
           "    }\n"
           "  },\n"
           "  \"Web\": {\n"
           "    \"${TS_CERT_DOMAIN}:443\": {\n"
           "      \"Handlers\": {\n"
           "        \"/\": {\n"
           "          \"Proxy\": \"" backend-url "\"\n"
           "        }\n"
           "      }\n"
           "    }\n"
           "  }\n"
           "}"))
         (serve-config-file
          (plain-file (string-append "ts-serve-" name ".json")
                      serve-config-content)))
    (oci-container-configuration
     (user "rafael")
     (image "tailscale/tailscale:latest")
     (provision (string-append "ts-" name))
     (requirement '(sops-secrets networking podman-prune))
     (respawn? #t)
     (ports ports)
     (volumes
      (list (string-append ts-state-dir ":/var/lib/tailscale")
            ;; Mount the whole tailscale secrets directory (not just the specific
            ;; file) so the container can start even when GPG decryption is still
            ;; running.  The directory always exists; the key file appears once sops
            ;; finishes.  The entrypoint waits for the file to be non-empty before
            ;; calling containerboot.
            "/run/secrets/tailscale:/run/secrets/tailscale:ro"
            (cons serve-config-file "/etc/tailscale/serve-config.json:ro")))
     (environment
      (list "TS_USERSPACE=true"
            "TS_STATE_DIR=/var/lib/tailscale"
            ;; TS_AUTHKEY_FILE is read by the entrypoint wrapper below and
            ;; re-exported as TS_AUTHKEY, which containerboot understands.
            (string-append "TS_AUTHKEY_FILE=/run/secrets/tailscale/"
                           secret-name "_authkey")
            ;; TS_SERVE_CONFIG instead of TS_SERVE_PORT: explicit serve config JSON
            ;; with backend-host avoids the netstack loopback routing issue.
            "TS_SERVE_CONFIG=/etc/tailscale/serve-config.json"
            ;; Register with a clean hostname (no "ts-" prefix — that's only for
            ;; shepherd service namespacing, not the Tailscale node name).
            (string-append "TS_HOSTNAME=" name)))
     ;; TS_USERSPACE=true uses gVisor netstack (no TUN device) so NET_ADMIN is not
     ;; needed and must be omitted: Podman 5.x passes -t none to pasta when
     ;; NET_ADMIN is present, silently breaking host→container port forwarding
     ;; on any restart (pasta only works at first boot otherwise).
     ;; Wrapper entrypoint: wait for the auth key file before calling containerboot.
     ;; sops GPG decryption can take >60 s on first boot; mounting the directory
     ;; (not just the key file) means podman starts the container immediately while
     ;; this loop waits for the key to arrive.
     ;;
     ;; tailscaled.state is persisted on the host at /data/tailscale/<name>/ so the
     ;; node reconnects as the same Tailscale machine on every reboot — no new node
     ;; registrations, no "navidrome-1 / navidrome-2 / ..." pollution.
     (entrypoint "/bin/sh")
     (command (list "-c"
                    (string-append
                     "while [ ! -s \"$TS_AUTHKEY_FILE\" ]; do sleep 1; done; "
                     "export TS_AUTHKEY=$(cat \"$TS_AUTHKEY_FILE\"); "
                     "exec /usr/local/bin/containerboot"))))))

(define (make-ts-ready-service ts-name)
  "Return a one-shot shepherd-service that polls the ts-TS-NAME sidecar
until tailscaled is actually authenticated and serving
(BackendState=Running), not merely until the podman container exists.

Previously this gate only checked `podman container exists`, which
succeeds the moment `podman run` returns — before containerboot has
read the authkey, brought tailscaled up, authenticated, or applied
the serve config.  Dependents then started while
`--network container:ts-<name>` pointed at a half-initialised netns
and failed with exit 125/126.

This version `podman exec`s into the sidecar and runs
`tailscale status --json --peers=false`.  The exec fails (non-zero)
while the container does not yet exist, and the JSON reports
BackendState != \"Running\" until auth completes.  Hard timeout 120 s:
on wedge the gate exits non-zero so dependents fail fast instead of
hanging forever (mcron watchdog will retry)."
  (let ((container-name (string-append "ts-" ts-name)))
    (simple-service
     (string->symbol (string-append "ts-" ts-name "-ready"))
     shepherd-root-service-type
     (list
      (shepherd-service
       (provision (list (string->symbol (string-append "ts-" ts-name "-ready"))))
       (requirement (list (string->symbol container-name)))
       (one-shot? #t)
       (start
        #~(make-forkexec-constructor
           (list
            #$(program-file
               (string-append "ts-" ts-name "-ready")
               #~(begin
                   (use-modules (ice-9 popen)
                                (ice-9 rdelim))
                   (let* ((pw   (getpwnam "rafael"))
                          (uid  (passwd:uid pw))
                          (gid  (passwd:gid pw))
                          (ruid (string-append "/run/user/"
                                               (number->string uid))))
                     (setenv "XDG_RUNTIME_DIR" ruid)
                     (setenv "HOME" (passwd:dir pw))
                     (setenv "PATH"
                             (string-append "/run/setuid-programs:"
                                            (or (getenv "PATH") "")))
                     (setgid gid)
                     (setuid uid)
                     (let ((podman   #$(file-append podman "/bin/podman"))
                           (deadline (+ (current-time) 120)))
                       (let loop ()
                         (let* ((port (open-pipe* OPEN_READ podman
                                                  "exec" #$container-name
                                                  "tailscale" "status"
                                                  "--json" "--peers=false"))
                                (out  (read-string port))
                                (rc   (status:exit-val (close-pipe port))))
                           ;; tailscale --json prints "BackendState":"Running"
                           ;; (compact) or "BackendState": "Running" (pretty).
                           ;; Accept either.
                           (if (and (zero? rc)
                                    (or (string-contains
                                         out "\"BackendState\":\"Running\"")
                                        (string-contains
                                         out "\"BackendState\": \"Running\"")))
                               #t  ; success → exit 0
                               (if (> (current-time) deadline)
                                   (begin
                                     (format (current-error-port)
                                             "ts-ready: ~a did not reach BackendState=Running within 120s~%"
                                             #$container-name)
                                     (exit 1))
                                   (begin
                                     (sleep 1)
                                     (loop)))))))))))
           #:log-file #$(string-append "/var/log/ts-" ts-name "-ready.log")))
       (documentation
        (string-append "Wait for " container-name
                       " to reach tailscale BackendState=Running.")))))))

(define* (make-app-container name image
                              #:key
                              (ts-name name)
                              (share-ts-netns? #t)
                              (ports '())
                              (volumes '())
                              (environment '())
                              (requirement '())
                              (extra-arguments '())
                              (entrypoint #f)
                              (command '()))
  "Return an oci-container-configuration sharing a Tailscale sidecar's network.
   NAME is the provision name.  TS-NAME is the sidecar to share network with.
   When SHARE-TS-NETNS? is #f, the container runs in its own pasta netns and
   PORTS (list of \"HOST:CONTAINER\" mappings) are published on the host.
   COMMAND overrides the image CMD (args passed to the container entrypoint).
   ENTRYPOINT overrides the image ENTRYPOINT when non-#f."
  (let ((base (if share-ts-netns?
                  (oci-container-configuration
                   (user "rafael")
                   (image image)
                   (provision name)
                   (requirement (cons* (string->symbol
                                        (string-append "ts-" ts-name "-ready"))
                                       'sops-secrets
                                       'podman-prune
                                       requirement))
                   (respawn? #t)
                   (ports ports)
                   (volumes volumes)
                   (environment environment)
                   (network (string-append "container:ts-" ts-name))
                   (extra-arguments extra-arguments)
                   (command command))
                  (oci-container-configuration
                   (user "rafael")
                   (image image)
                   (provision name)
                   ;; Standalone pasta netns: no shared ts sidecar, no secrets
                   ;; consumed, so don't depend on sops-secrets — otherwise every
                   ;; respawn churns the sops secret chain and cascades.
                   (requirement (cons* 'podman-prune requirement))
                   (respawn? #t)
                   (ports ports)
                   (volumes volumes)
                   (environment environment)
                   (extra-arguments extra-arguments)
                   (command command)))))
    (if entrypoint
        (oci-container-configuration
         (inherit base)
         (entrypoint entrypoint))
        base)))

;;;
;;; Habitica image pin
;;;
;;; Upstream HabitRPG/habitica has no published OCI image; we build locally on
;;; lovelace via scripts/build-habitica-image.sh, tagging with this commit SHA.
;;; Bump the pin → re-run the build script on lovelace → guix deploy.
(define %habitica-commit "a92999fc11a0fcfe24d74ddc952219ece5d73101")

;;;
;;; habitica-rs-init: one-shot that calls rs.initiate(...) on the mongo
;;; container after first start. Idempotent — rs.status() succeeds on
;;; subsequent boots and the script becomes a no-op. Required because
;;; Habitica's server uses MongoDB change-streams, which only work when
;;; the database is configured as a replica set.
;;;
(define habitica-rs-init-service
  (simple-service
   'habitica-rs-init
   shepherd-root-service-type
   (list
    (shepherd-service
     (provision '(habitica-rs-init))
     (requirement '(habitica-mongo))
     (one-shot? #t)
     (start
      #~(make-forkexec-constructor
         (list
          #$(program-file
             "habitica-rs-init"
             #~(begin
                 (use-modules (ice-9 popen)
                              (ice-9 rdelim))
                 (let* ((pw   (getpwnam "rafael"))
                        (uid  (passwd:uid pw))
                        (gid  (passwd:gid pw))
                        (ruid (string-append "/run/user/"
                                             (number->string uid))))
                   (setenv "XDG_RUNTIME_DIR" ruid)
                   (setenv "HOME" (passwd:dir pw))
                   (setenv "PATH"
                           (string-append "/run/setuid-programs:"
                                          (or (getenv "PATH") "")))
                   (setgid gid)
                   (setuid uid)
                   (let ((podman   #$(file-append podman "/bin/podman"))
                         (deadline (+ (current-time) 120))
                         (script   "try { rs.status() } catch (e) { rs.initiate({_id:'rs', members:[{_id:0, host:'127.0.0.1:27017'}]}) }"))
                     (let loop ()
                       (let* ((port (open-pipe* OPEN_READ podman
                                                "exec" "habitica-mongo"
                                                "mongosh" "--quiet"
                                                "--eval" script))
                              (out  (read-string port))
                              (rc   (status:exit-val (close-pipe port))))
                         (cond
                          ((zero? rc)
                           (format #t "habitica-rs-init: ~a~%" out)
                           #t)
                          ((> (current-time) deadline)
                           (format (current-error-port)
                                   "habitica-rs-init: mongosh did not succeed within 120s~%")
                           (exit 1))
                          (else
                           (sleep 2)
                           (loop))))))))))
         #:log-file "/var/log/habitica-rs-init.log"))
     (documentation
      "Initialize MongoDB replica set 'rs' for the Habitica server (idempotent).")))))

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
          ;; host.containers.internal (pasta gateway 169.254.1.2) routes to
          ;; the real host; 192.168.88.46 routes to the container's own loopback.
          "DB_HOST=host.containers.internal"
          "DB_PORT=5432"
          "DB_BASE=freshrss"
          "DB_USER=freshrss")
    ;; FreshRSS needs DB_PASSWORD as a plain env var (no native _FILE support).
    ;; Pass the image CMD as args to the entrypoint so `exec "$@"` starts apache2.
    ;; Image CMD: /bin/bash -o pipefail -c "([ -z $CRON_MIN ] || cron) && . /etc/apache2/envvars && exec apache2 -D FOREGROUND"
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export DB_PASSWORD=$(cat /run/secrets/db_password); exec ./Docker/entrypoint.sh /bin/bash -o pipefail -c '([ -z \"$CRON_MIN\" ] || cron) && . /etc/apache2/envvars && exec apache2 -D FOREGROUND'"))

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
          "SYMFONY__ENV__DATABASE_HOST=host.containers.internal"
          "SYMFONY__ENV__DATABASE_PORT=5432"
          "SYMFONY__ENV__DATABASE_NAME=wallabag"
          "SYMFONY__ENV__DATABASE_USER=wallabag"
          "SYMFONY__ENV__DOMAIN_NAME=https://wallabag.drake-karat.ts.net")
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
    (list "--cap-drop=ALL" "--cap-add=CHOWN" "--cap-add=SETGID" "--cap-add=SETUID"))

   ;; ── SearxNG (kids) ────────────────────────────────────────────────────
   ;; Parallel SearxNG instance with strict SafeSearch + reduced engine set,
   ;; on its own Tailscale node so kids' devices browse it via MagicDNS at
   ;; http://searxng-kids:8080.  Settings live at /data/searxng-kids/.
   ;; Shares the secret_key with the adult instance — the key is just for
   ;; session-state HMAC; no privacy advantage to a separate one.
   ;; Shares the same Tailscale auth key as the main searxng node — needs
   ;; to be REUSABLE in Tailscale admin (re-generate if it was single-use).
   ;; Port 8081 (not 8080) because make-ts-sidecar routes via host LAN IP
   ;; and main searxng already claims host port 8080 via pasta.
   (make-ts-sidecar "searxng-kids" #:serve-port 8081 #:secret-name "searxng")
   (make-app-container
    "searxng-kids" "searxng/searxng:latest"
    #:volumes
    (list "/data/searxng-kids:/etc/searxng:rw"
          "/run/secrets/searxng/secret_key:/run/secrets/secret_key:ro")
    #:environment (list "SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml"
                        "SEARXNG_PORT=8081"
                        "SEARXNG_BIND_ADDRESS=0.0.0.0")
    #:extra-arguments
    (list "--cap-drop=ALL" "--cap-add=CHOWN" "--cap-add=SETGID" "--cap-add=SETUID"))

   ;; ── Habitica ──────────────────────────────────────────────────────────
   ;; Three-container stack sharing one Tailscale netns:
   ;;   ts-habitica   — tailnet TLS termination, proxies :443 → :3000
   ;;   habitica-mongo — mongo:7.0, --replSet rs, binds 127.0.0.1:27017
   ;;   habitica       — locally built image (see scripts/build-habitica-image.sh),
   ;;                    serves API + built SPA on :3000
   ;; A separate one-shot (habitica-rs-init) calls rs.initiate(...) once after
   ;; mongo first comes up; see habitica-rs-init-service below.
   ;; The image tag is pinned to %habitica-commit so deploys are reproducible:
   ;; bumping the pin requires re-running build-habitica-image.sh on lovelace.
   (make-ts-sidecar "habitica" #:serve-port 3000)
   (make-app-container
    "habitica-mongo" "docker.io/library/mongo:7.0"
    #:ts-name "habitica"
    #:volumes (list "/data/habitica/db:/data/db")
    ;; --bind_ip 127.0.0.1: mongo only reachable inside the shared netns
    ;; (i.e. by the habitica server container), never on the tailnet.
    ;; --replSet rs: required by the Habitica server (uses change-streams).
    #:command (list "--replSet" "rs" "--bind_ip" "127.0.0.1"))
   (make-app-container
    "habitica" (string-append "localhost/habitica:" %habitica-commit)
    #:ts-name "habitica"
    #:requirement '(habitica-rs-init)
    #:volumes
    (list "/run/secrets/habitica/session_secret:/run/secrets/session_secret:ro"
          "/run/secrets/habitica/session_secret_key:/run/secrets/session_secret_key:ro")
    #:environment
    (list "NODE_ENV=production"
          "PORT=3000"
          "BASE_URL=https://habitica.drake-karat.ts.net"
          "TRUSTED_DOMAINS=https://habitica.drake-karat.ts.net"
          "ADMIN_EMAIL=rafpal@ous-hf.no"
          ;; directConnection=true: single-node replica set; skip topology probe.
          (string-append "NODE_DB_URI=mongodb://127.0.0.1:27017/habitrpg"
                         "?replicaSet=rs&directConnection=true"))
    ;; Read session secrets from sops-mounted files at boot. WORKDIR in
    ;; Dockerfile-Dev is /usr/src/habitica; `npm start` runs gulp's prod server.
    ;; The sed below is a runtime patch: upstream index.js gates @babel/register
    ;; on NODE_ENV != production, expecting a pre-transpiled tree.  We run npm
    ;; start against the source, so under Node 20 ESM-detect setupNconf.js
    ;; loads as a real ES module and crashes on `__dirname`.  Forcing the
    ;; babel runtime hook unconditionally avoids that without a 2.4 GB image
    ;; rebuild.  Same patch is also applied at build time in
    ;; scripts/build-habitica-image.sh; this keeps existing images working.
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    (string-append
                     "export SESSION_SECRET=$(cat /run/secrets/session_secret); "
                     "export SESSION_SECRET_KEY=$(cat /run/secrets/session_secret_key); "
                     "cd /usr/src/habitica && "
                     "sed -i \"s/^if (process.env.NODE_ENV !== 'production') {/if (true) {/\" website/server/index.js && "
                     "exec npm start")))))

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
    (requirement '(networking))
    (respawn? #t)
    (volumes
     (list "/data/gluetun-pihole:/gluetun"
           "/run/secrets/mullvad/pihole_wg_private_key:/run/secrets/wg-key:ro"
           "/run/secrets/mullvad/pihole_wg_address:/run/secrets/wg-address:ro"))
    (environment
     (list "VPN_SERVICE_PROVIDER=mullvad"
           "VPN_TYPE=wireguard"))
    ;; Gluetun does not support WIREGUARD_PRIVATE_KEY_FILE or WIREGUARD_ADDRESSES_FILE;
    ;; read both from sops-managed secret files so they can be updated without redeploying.
    (entrypoint "/bin/sh")
    (command (list "-c"
                   "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); export WIREGUARD_ADDRESSES=$(cat /run/secrets/wg-address); exec /gluetun-entrypoint"))
    ;; Publish pihole's web UI (port 80) as host port 8053.
    ;; Port 8000 in this netns is gluetun's HTTP control API — not pihole.
    (ports (list "53:53/tcp" "53:53/udp" "0.0.0.0:8053:80"))
    (extra-arguments (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun")))

   (oci-container-configuration
    (user "rafael")
    (image "pihole/pihole:latest")
    (provision "pihole")
    (requirement '(gluetun-pihole))
    (respawn? #t)
    (volumes
     (list "/data/pihole/etc:/etc/pihole"
           "/data/pihole/dnsmasq:/etc/dnsmasq.d"
           "/run/secrets/pihole/webpassword:/run/secrets/webpassword:ro"))
    (environment
     (list
      ;; WEBPASSWORD_FILE is a filename relative to /run/secrets/.
      ;; bash_functions.sh reads /run/secrets/$WEBPASSWORD_FILE and
      ;; exports it as FTLCONF_webserver_api_password so FTL applies
      ;; the password hash on every container start.
      "WEBPASSWORD_FILE=webpassword"
      "TZ=Europe/Oslo"
      "DNSMASQ_LISTENING=all"
      "FTLCONF_webserver_serve_all=true"))
    (network "container:gluetun-pihole"))

   (make-ts-sidecar "pihole" #:serve-port 8053
                    ;; pihole shares gluetun-pihole's netns, not ts-pihole's.
                    ;; Use host.containers.internal (pasta gateway 169.254.1.2)
                    ;; so pasta routes to the real host, where gluetun-pihole
                    ;; publishes port 8053→80 (pihole's web UI).
                    #:backend-host "host.containers.internal"
                    #:ts-state-dir "/data/tailscale/pihole")

   ;; ── qBittorrent ───────────────────────────────────────────────────────
   (oci-container-configuration
    (user "rafael")
    (image "qmcgaw/gluetun:latest")
    (provision "gluetun-qbt")
    (requirement '(networking))
    (respawn? #t)
    (volumes
     (list "/data/gluetun-qbt:/gluetun"
           "/run/secrets/mullvad/qbt_wg_private_key:/run/secrets/wg-key:ro"
           "/run/secrets/mullvad/qbt_wg_address:/run/secrets/wg-address:ro"))
    (environment
     (list "VPN_SERVICE_PROVIDER=mullvad"
           "VPN_TYPE=wireguard"))
    ;; Read both private key and address from sops secrets — updateable without redeploy.
    (entrypoint "/bin/sh")
    (command (list "-c"
                   "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); export WIREGUARD_ADDRESSES=$(cat /run/secrets/wg-address); exec /gluetun-entrypoint"))
    ;; 0.0.0.0 so pasta (ts-qbt) can reach it via host.containers.internal.
    ;; 127.0.0.1 only would be unreachable from ts-qbt's pasta namespace.
    (ports (list "0.0.0.0:8080:8080"))
    (extra-arguments (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun")))

   (oci-container-configuration
    (user "rafael")
    (image "lscr.io/linuxserver/qbittorrent:latest")
    (provision "qbittorrent")
    (requirement '(gluetun-qbt))
    (respawn? #t)
    (volumes
     (list "/data/qbittorrent/config:/config"
           "/data/qbittorrent/downloads:/downloads"))
    (environment
     (list "PUID=1000" "PGID=1000" "TZ=Europe/Oslo" "WEBUI_PORT=8080"))
    (network "container:gluetun-qbt"))

   (make-ts-sidecar "qbt" #:serve-port 8080
                    ;; Same reason as pihole: qbittorrent shares gluetun-qbt's
                    ;; netns; use host.containers.internal to reach the host.
                    #:backend-host "host.containers.internal"
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
;;; Uses host.containers.internal to reach prometheus (on host network :9090).
;;; 192.168.88.46 fails from pasta network namespace due to hairpin NAT;
;;; host.containers.internal is set by Podman to the pasta gateway IP.
(define %grafana-prometheus-datasource
  (plain-file "prometheus-datasource.yaml"
              "apiVersion: 1
datasources:
  - name: Prometheus
    type: prometheus
    url: http://host.containers.internal:9090
    isDefault: true
    access: proxy
    editable: false
"))

(define %monitoring-containers
  (list
   ;; ── smartctl-exporter ─────────────────────────────────────────────────
   ;; No TS sidecar — scraped internally by Prometheus on the host network.
   ;; Runs as rootful podman (user "root") so the container has real uid 0
   ;; and can access block devices for SMART health data.
   (oci-container-configuration
    (user "root")
    (container-user "root")
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
   ;; ts-prometheus exposes Prometheus on Tailscale at prometheus.drake-karat.ts.net.
   ;; Prometheus is on host network (:9090), reachable at 192.168.88.46:9090.
   (make-ts-sidecar "prometheus" #:serve-port 9090
                    ;; prometheus runs on --network=host (separate from ts-prometheus).
                    ;; Use host.containers.internal so pasta routes to the real host.
                    #:backend-host "host.containers.internal")
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
   (make-ts-sidecar "grafana" #:serve-port 3000)
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
          "GF_SERVER_HTTP_PORT=3000")
    ;; Run as container root (= host rafael uid 1000) so it can write to
    ;; /data/grafana, which is owned by rafael.  Grafana uid 472 cannot
    ;; write to a directory owned by container-root in the user namespace.
    #:extra-arguments '("--user=0"))))

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
  (append
   ;; Gate services: one-shot readiness checks that ensure each ts-* sidecar
   ;; container is registered in podman before the app container tries
   ;; --network=container:ts-<name>.  Without these, app containers race
   ;; against their sidecar's `podman run` and fail with "no container found".
   (list (make-ts-ready-service "freshrss")
         (make-ts-ready-service "nextcloud")
         (make-ts-ready-service "wallabag")
         (make-ts-ready-service "rss-bridge")
         (make-ts-ready-service "searxng")
         (make-ts-ready-service "searxng-kids")
         (make-ts-ready-service "grafana")
         (make-ts-ready-service "habitica"))
   (list
    (service oci-service-type
             (oci-configuration
              (runtime 'podman)
              (containers (append %app-containers
                                  %vpn-containers
                                  %monitoring-containers)))))))
