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
  ;; Hide upstream luanti-mobs / luanti-mobs-monster so our newer
  ;; mineclonia-compatible overrides in (entelequia packages games) win.
  #:use-module ((gnu packages luanti)
                #:hide (luanti-mobs luanti-mobs-monster))
  #:use-module (gnu packages linux)
  #:use-module (entelequia packages games)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (postgresql-lovelace-service
            smartd-lovelace-service
            luanti-game-service
            starbound-game-service
            heroes-server-game-service
            borgmatic-lovelace-service
            lovelace-data-dir-service
            searxng-settings-service
            nextcloud-proxy-config-service
            nextcloud-provision-service
            lovelace-container-services
            lovelace-nfs-service
            podman-prune-service
            make-podman-shepherd-service
            make-ts-sidecar
            make-ts-ready-service
            make-app-container
            ts-netns-watchdog-service))

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
                          ;; /data/nextcloud/config: non-recursive chown to rafael so
                          ;; container root (= host rafael in rootless Podman) can write.
                          ;; Subdirs are owned by container abc (host uid 232071); do NOT
                          ;; chown recursively or the web server loses access.
                          ;; /data/nextcloud/data is NOT listed: container init owns it.
                          "/data/nextcloud/config"
                          "/data/borg"))))))

;;;
;;; searxng-settings-service: make the SearxNG tuning declarative + reproducible.
;;; The image generates a full settings.yml in /data/searxng{,-kids}; rather than
;;; reconstruct 2700 lines (and risk the secret_key + engine list), this activation
;;; service idempotently ENFORCES the two settings we care about on both instances,
;;; on every reconfigure/boot:
;;;   - outgoing request_timeout 3.0 -> 8.0 (+ max_request_timeout 15.0): the 3s
;;;     default is too tight through the Mullvad exit, so engines time out and
;;;     searches flake.  Applies to BOTH instances (kids search benefits too).
;;;   - search.formats: add `json` (needed by the searxng-mcp server Poppins uses;
;;;     HTML-only returns 403 for ?format=json).
;;; Idempotent: each line is matched exactly, so the timeout edits only fire on
;;; the old defaults, and json is added only when absent.  Pure procedures (no
;;; substitute* macro — that needs compile-time module imports an activation
;;; gexp doesn't get).  Runs at activation (before the containers start on boot);
;;; on a live reconfigure restart searxng{,-kids} once so they re-read settings.yml.
(define searxng-settings-service
  (list
   (simple-service 'searxng-settings
                   activation-service-type
                   #~(begin
                       (use-modules (ice-9 textual-ports))
                       (define (patch p)
                         (when (file-exists? p)
                           (let* ((content (call-with-input-file p get-string-all))
                                  (lines   (string-split content #\newline))
                                  (has-json (and (member "    - json" lines) #t))
                                  (out '()))
                             (for-each
                              (lambda (l)
                                (cond
                                 ((string=? l "  request_timeout: 3.0")
                                  (set! out (cons "  request_timeout: 8.0" out)))
                                 ((string=? l "  # max_request_timeout: 10.0")
                                  (set! out (cons "  max_request_timeout: 15.0" out)))
                                 ((and (string=? l "  formats:") (not has-json))
                                  (set! out (cons "    - json" (cons l out))))
                                 (else (set! out (cons l out)))))
                              lines)
                             (call-with-output-file p
                               (lambda (port)
                                 (put-string port (string-join (reverse out) "\n")))))))
                       (for-each patch
                                 (list "/data/searxng/settings.yml"
                                       "/data/searxng-kids/settings.yml"))))))

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
;;; nextcloud-provision: idempotent one-shot that provisions FAMILY content
;;; INSIDE the already-running `nextcloud` container — it does NOT install or
;;; replace NextCloud.  Runs occ via `podman exec --user abc nextcloud php
;;; /app/www/public/occ ...` (occ lives at /app/www/public/occ in the lsio
;;; image; the container user `abc` = host uid 232071).
;;;
;;; RECONCILED with the live instance (inspected 2026-06-02): users Maria,
;;; Leandro, rafael and ncadmin ALREADY exist.  This service NEVER recreates
;;; them — it only (a) enables apps, (b) creates the missing accounts (Adrian +
;;; the two agents), (c) creates groups + group folders, (d) re-asserts group
;;; membership.  All idempotent and non-destructive: no existing data is touched.
;;;
;;; Photos (ADR-0008): enables `memories` (NextCloud-native photos, fed by the
;;; mobile app's auto-upload).  `recognize` (face/object ML) is DEFERRED —
;;; enable by hand later, off-peak (RAM-heavy on lovelace's 7.5 GB).
;;;
;;; ACTIVATION (operator): the 3 seed-password sops decls in lovelace.scm are
;;; shipped COMMENTED OUT, so this service stays INERT (unmet requirement) and
;;; deploying before activation is SAFE.  To activate, do these together: add the
;;; 3 values under `nextcloud:` in sops/lovelace.yaml (userpw_Adrian,
;;; userpw_mary-poppins, userpw_arquimedes) AND uncomment the matching decls in
;;; lovelace.scm, then deploy.  The seeds are read once, as root, BEFORE
;;; privileges drop, so they stay #o400 root-only, are never mounted into the
;;; container, and OC_PASS is forwarded into the container BY NAME (not value) so
;;; it never appears in the host argv / process table.
(define %nextcloud-provision-script
  (program-file
   "nextcloud-provision"
   #~(begin
       (use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 textual-ports)
                    (ice-9 regex) (srfi srfi-1) (srfi srfi-13))
       ;; seed-password plumbing for the to-be-created accounts.
       (define %seed-ids '("Adrian" "mary-poppins" "arquimedes"))
       (define (seed-path id)
         (string-append "/run/secrets/nextcloud/userpw_" id))
       (define (read-seed id)
         (string-trim-both (call-with-input-file (seed-path id) get-string-all)))
       ;; INERT UNTIL ACTIVATED: while the seed decls in lovelace.scm are still
       ;; commented out, /run/secrets/nextcloud/userpw_* don't exist -> exit 0
       ;; cleanly (no error, no sops cascade).  Activation (operator) adds the
       ;; yaml values + uncomments the decls; then the seeds appear and this runs.
       (unless (every (lambda (id) (file-exists? (seed-path id))) %seed-ids)
         (format #t "nextcloud-provision: seed passwords absent (not activated); skipping~%")
         (exit 0))
       ;; (as root) slurp the seeds BEFORE dropping privileges, so the sops files
       ;; can stay #o400 root-only; then drop to rafael (rootless-podman owner).
       (let* ((seed-pw (map (lambda (id) (cons id (read-seed id))) %seed-ids))
              (pw   (getpwnam "rafael"))
              (uid  (passwd:uid pw))
              (gid  (passwd:gid pw))
              (ruid (string-append "/run/user/" (number->string uid)))
              (provdir "/var/lib/nextcloud-provision"))
         ;; Create the handoff dir as ROOT (rafael can't mkdir under root-owned
         ;; /var/lib), owned by rafael, 0700 so token files aren't listable.
         (unless (file-exists? provdir) (mkdir provdir))
         (chown provdir uid gid)
         (chmod provdir #o700)
         (setenv "XDG_RUNTIME_DIR" ruid)
         (setenv "HOME" (passwd:dir pw))
         (setenv "PATH" "/run/setuid-programs:/run/current-system/profile/bin")
         (setgid gid)
         (setuid uid)
         (chdir provdir)   ; rafael-owned; avoids "cannot chdir to /root" popen noise
         (let* ((podman  #$(file-append podman "/bin/podman"))
                (occbin  "/app/www/public/occ")
                ;; (id display-name . groups).  Maria/rafael/Leandro already
                ;; exist (group-only); Adrian + the two agents get created.
                (users
                 (list (list "Maria"        "Maria"        "family" "parents")
                       (list "rafael"       "Rafael"       "family" "parents")
                       (list "Leandro"      "Leandro"      "family" "kids")
                       (list "Adrian"       "Adrian"       "family" "kids")
                       (list "mary-poppins" "Mary Poppins" "family" "agents")
                       (list "arquimedes"   "Arquimedes"   "kids"   "agents")))
                (agents '("mary-poppins" "arquimedes")))

           ;; podman exec --user abc nextcloud php occ -n ARGS... -> stdout string
           ;; (-n/--no-interaction: never block on a prompt under non-TTY exec.)
           (define (occ . args)
             (let* ((cmd  (append (list podman "exec" "--user" "abc"
                                        "nextcloud" "php" occbin "-n") args))
                    (port (apply open-pipe* OPEN_READ cmd))
                    (out  (read-string port)))
               (close-pipe port)
               (if (eof-object? out) "" out)))
           ;; ...with extra ENV + exit code.  env-pairs are "KEY=VALUE": we set
           ;; them in OUR env and forward by NAME (-e KEY), so a secret value
           ;; (OC_PASS) NEVER lands in the host argv / process table.
           (define (occ-env/rc env-pairs . args)
             (for-each (lambda (kv)
                         (let ((i (string-index kv #\=)))
                           (setenv (substring kv 0 i) (substring kv (1+ i)))))
                       env-pairs)
             (let* ((envargs (append-map
                              (lambda (kv)
                                (list "-e" (substring kv 0 (string-index kv #\=))))
                              env-pairs))
                    (cmd  (append (list podman "exec") envargs
                                  (list "--user" "abc" "nextcloud" "php" occbin "-n")
                                  args))
                    (port (apply open-pipe* OPEN_READ cmd))
                    (out  (read-string port))
                    (rc   (status:exit-val (close-pipe port))))
               (for-each (lambda (kv)
                           (unsetenv (substring kv 0 (string-index kv #\=))))
                         env-pairs)
               (values (if (eof-object? out) "" out) rc)))
           ;; create a group folder keyed on a group, idempotent by mountPoint.
           ;; Probe the real "mountPoint":"<mount>" field; on create require rc=0
           ;; AND a bare-numeric id before binding the group (a failed create
           ;; otherwise emits an <error> sentence we must not treat as an id).
           (define (ensure-groupfolder mount group)
             (let ((gf (occ "groupfolders:list" "--output=json")))
               (unless (string-contains
                        gf (string-append "\"mountPoint\":\"" mount "\""))
                 (call-with-values
                     (lambda () (occ-env/rc '() "groupfolders:create" mount))
                   (lambda (out rc)
                     (let ((id (string-trim-both out)))
                       (if (and (zero? rc) (> (string-length id) 0)
                                (string-every char-numeric? id))
                           ;; groupfolders 21 takes positional permission WORDS
                           ;; (read write create delete share), NOT --flags.
                           (occ "groupfolders:group" id group
                                "read" "write" "delete")
                           (format (current-error-port)
                                   "nextcloud-provision: WARN groupfolders:create ~a rc=~a out=~s~%"
                                   mount rc id))))))))

           ;; (0) wait until NextCloud reports installed:true (compact json)
           (let loop ((deadline (+ (current-time) 300)))
             (let ((s (occ "status" "--output=json")))
               (cond
                ((string-contains s "\"installed\":true")
                 (format #t "nextcloud-provision: NextCloud ready~%"))
                ((> (current-time) deadline)
                 (format (current-error-port)
                         "nextcloud-provision: NextCloud not ready within 300s~%")
                 (exit 1))
                (else (sleep 3) (loop deadline)))))

           ;; (1) enable apps (app:enable is a no-op if already on).  Check rc:
           ;; groupfolders is FATAL if it won't enable (step 4 depends on it);
           ;; deck/memories only warn.
           (for-each
            (lambda (a)
              (call-with-values (lambda () (occ-env/rc '() "app:enable" a))
                (lambda (out rc)
                  (unless (zero? rc)
                    (if (string=? a "groupfolders")
                        (begin
                          (format (current-error-port)
                                  "nextcloud-provision: FATAL app:enable groupfolders rc=~a~%" rc)
                          (exit 1))
                        (format (current-error-port)
                                "nextcloud-provision: WARN app:enable ~a rc=~a~%" a rc))))))
            '("deck" "groupfolders" "memories"))

           ;; (1b) Calendar app (NOT bundled) — install (downloads + enables) if
           ;; absent.  Required for CalDAV calendar SHARING (the sabre sharing
           ;; plugin lives in this app) + the family-calendar share in step 8.
           ;; Idempotent: guard on app:list (app:install errors if already there).
           (unless (string-contains (occ "app:list" "--output=json") "\"calendar\"")
             (call-with-values (lambda () (occ-env/rc '() "app:install" "calendar"))
               (lambda (out rc)
                 (unless (zero? rc)
                   (format (current-error-port)
                           "nextcloud-provision: WARN app:install calendar rc=~a~%" rc)))))

           ;; (2) groups (group:add errors on dup -> probe group:list first)
           (let ((groups (occ "group:list" "--output=json")))
             (for-each
              (lambda (g)
                (unless (string-contains groups (string-append "\"" g "\":"))
                  (occ "group:add" g)))
              '("family" "parents" "kids" "agents")))

           ;; (3) users — create only the absent ones (Adrian + agents); existing
           ;; Maria/rafael/Leandro are skipped.  Group membership re-asserted for
           ;; everyone (group:adduser is idempotent).  OC_PASS from the sops seed.
           (let ((existing (occ "user:list" "--output=json")))
             (for-each
              (lambda (u)
                (let* ((id   (car u))
                       (disp (cadr u))
                       (grps (cddr u)))
                  (unless (string-contains existing (string-append "\"" id "\":"))
                    (let ((pwval
                           (cond ((assoc id seed-pw) => cdr)
                                 (else
                                  (format (current-error-port)
                                          "nextcloud-provision: FATAL no seed for ~a~%" id)
                                  (exit 1)))))
                      (call-with-values
                          (lambda ()
                            (apply occ-env/rc
                                   (list (string-append "OC_PASS=" pwval))
                                   "user:add" id "--display-name" disp
                                   "--password-from-env"
                                   (append-map (lambda (g) (list "--group" g)) grps)))
                        (lambda (out rc)
                          (unless (zero? rc)
                            (format (current-error-port)
                                    "nextcloud-provision: FATAL user:add ~a rc=~a~%"
                                    id rc)
                            (exit 1))
                          (format #t "nextcloud-provision: created user ~a~%" id)))))
                  (for-each
                   (lambda (g)
                     (call-with-values (lambda () (occ-env/rc '() "group:adduser" g id))
                       (lambda (out rc)
                         (unless (zero? rc)
                           (format (current-error-port)
                                   "nextcloud-provision: WARN group:adduser ~a ~a rc=~a~%"
                                   g id rc)))))
                   grps)))
              users))

           ;; (4) group folders — keyed on group membership (mary-poppins, in
           ;; `family`, sees Family; arquimedes, in `kids` only, sees Kids).
           (ensure-groupfolder "Family" "family")
           (ensure-groupfolder "Kids"   "kids")

           ;; (5) shared family calendar + addressbook on rafael (best-effort:
           ;; dav:create-* errors on dup; rc ignored).
           (occ "dav:create-calendar"    "rafael" "family")
           (occ "dav:create-addressbook" "rafael" "family-contacts")

           ;; (6) files:scan intentionally OMITTED — group folders are created via
           ;; occ (which indexes them) and no host dirs are seeded; `files:scan
           ;; --all` is slow on a large instance (169 GB here) and unnecessary.

           ;; (7) agent app-passwords — print-once -> file-now handoff (0600,
           ;; owner rafael).  Mint FULL-capability tokens (OC_PASS via the seed,
           ;; forwarded by name) with the canonical verb; rc-check + validate the
           ;; token shape before writing, so a stray stdout word can't poison the
           ;; idempotency gate (a bad parse leaves the gate empty -> re-run retries).
           (for-each
            (lambda (agent)
              (let ((apf  (string-append provdir "/" agent ".app-password"))
                    (seed (assoc agent seed-pw)))
                (when (and seed
                           (not (and (file-exists? apf)
                                     (> (stat:size (stat apf)) 0))))
                  (call-with-values
                      (lambda ()
                        (occ-env/rc (list (string-append "OC_PASS=" (cdr seed)))
                                    "user:auth-tokens:add" agent "--password-from-env"))
                    (lambda (out rc)
                      (let* ((ws  (string-tokenize out))
                             (tok (and (pair? ws) (last ws))))
                        (if (and (zero? rc) tok (>= (string-length tok) 20)
                                 (string-every
                                  (lambda (c) (or (char-alphabetic? c)
                                                  (char-numeric? c))) tok))
                            (begin
                              (call-with-output-file apf
                                (lambda (p) (display tok p)))
                              (chown apf uid gid)
                              (chmod apf #o600)
                              (format #t "nextcloud-provision: wrote ~a~%" apf))
                            (format (current-error-port)
                                    "nextcloud-provision: WARN app-password ~a rc=~a (not written; will retry)~%"
                                    agent rc))))))))
            agents)

           ;; (8) Mary Poppins's shared family calendar — she OWNS it (writes via
           ;; the cbcoutinho NextCloud MCP sidecar) and it is SHARED to the `family`
           ;; group so everyone sees it.  NextCloud has NO occ verb to create a
           ;; calendar share, and sabre only accepts the owncloud-ns oc:share POST
           ;; (cs:/JSON bodies return 501) — so POST it as mary-poppins with her
           ;; app-pw.  Idempotent: create errors on dup (ignored, like rafael's);
           ;; the share is guarded by dav:list-calendar-shares.
           (let ((mp-tok (string-append provdir "/mary-poppins.app-password")))
             (when (and (file-exists? mp-tok) (> (stat:size (stat mp-tok)) 0))
               (occ "dav:create-calendar" "mary-poppins" "family")
               (unless (string-contains
                        (occ "dav:list-calendar-shares" "mary-poppins")
                        "principals/groups/family")
                 (let* ((tok (string-trim-both
                              (call-with-input-file mp-tok get-string-all)))
                        (rc  (status:exit-val
                              (system* podman "exec" "nextcloud" "curl"
                                       "-sS" "-f" "-o" "/dev/null"
                                       "-u" (string-append "mary-poppins:" tok)
                                       "-X" "POST"
                                       "http://127.0.0.1:80/remote.php/dav/calendars/mary-poppins/family"
                                       "-H" "Content-Type: application/xml; charset=utf-8"
                                       "--data" "<?xml version=\"1.0\"?><oc:share xmlns:d=\"DAV:\" xmlns:oc=\"http://owncloud.org/ns\"><oc:set><d:href>principal:principals/groups/family</d:href><oc:read-write/></oc:set></oc:share>"))))
                   (if (zero? rc)
                       (format #t "nextcloud-provision: shared mary-poppins/family calendar -> family group~%")
                       (format (current-error-port)
                               "nextcloud-provision: WARN calendar share rc=~a~%" rc))))))

           ;; (9) Mary Poppins's shared family Deck board — she OWNS it (writes via
           ;; the cbcoutinho MCP) and it is SHARED to the `family` group with EDIT,
           ;; mirroring step (8) for the calendar.  Deck has no occ verb, so create
           ;; + ACL go through the Deck REST API as mary-poppins.  Idempotent AND
           ;; non-invasive: if a board titled "Family" is already visible to her
           ;; (e.g. a human-owned board shared to her), leave its sharing to the
           ;; human and do nothing; only on a fresh instance do we create the board
           ;; and grant the `family` group edit.  ACL participant type 1 = group.
           (let ((mp-tok (string-append provdir "/mary-poppins.app-password")))
             (when (and (file-exists? mp-tok) (> (stat:size (stat mp-tok)) 0))
               (let* ((tok  (string-trim-both
                             (call-with-input-file mp-tok get-string-all)))
                      (base "http://127.0.0.1:80/index.php/apps/deck/api/v1.0")
                      ;; capture curl stdout (run inside the container as mary-poppins)
                      (deck-curl
                       (lambda args
                         (let* ((cmd  (append
                                       (list podman "exec" "nextcloud" "curl" "-sS"
                                             "-u" (string-append "mary-poppins:" tok)
                                             "-H" "OCS-APIRequest: true")
                                       args))
                                (port (apply open-pipe* OPEN_READ cmd))
                                (out  (get-string-all port)))
                           (close-pipe port)
                           (if (eof-object? out) "" out)))))
                 (let ((boards (deck-curl "-X" "GET"
                                          (string-append base "/boards"))))
                   (if (string-contains boards "\"title\":\"Family\"")
                       (format #t "nextcloud-provision: Family Deck board already present; leaving sharing to the human~%")
                       (let* ((created (deck-curl
                                        "-X" "POST"
                                        "-H" "Content-Type: application/json"
                                        "--data" "{\"title\":\"Family\",\"color\":\"0082c9\"}"
                                        (string-append base "/boards")))
                              (m   (string-match "\"id\":([0-9]+)" created))
                              (bid (and m (match:substring m 1))))
                         (if (not bid)
                             (format (current-error-port)
                                     "nextcloud-provision: WARN Deck board create failed: ~s~%" created)
                             (let ((rc (status:exit-val
                                        (system* podman "exec" "nextcloud" "curl"
                                                 "-sS" "-f" "-o" "/dev/null"
                                                 "-u" (string-append "mary-poppins:" tok)
                                                 "-H" "OCS-APIRequest: true"
                                                 "-H" "Content-Type: application/json"
                                                 "-X" "POST"
                                                 "--data" "{\"type\":1,\"participant\":\"family\",\"permissionEdit\":true,\"permissionShare\":false,\"permissionManage\":false}"
                                                 (string-append base "/boards/" bid "/acl")))))
                               (if (zero? rc)
                                   (format #t "nextcloud-provision: created+shared Family Deck board ~a -> family (edit)~%" bid)
                                   (format (current-error-port)
                                           "nextcloud-provision: WARN Deck ACL rc=~a~%" rc))))))))))

           (format #t "nextcloud-provision: done~%"))))))

(define nextcloud-provision-service
  (simple-service
   'nextcloud-provision
   shepherd-root-service-type
   (list
    (shepherd-service
     (provision '(nextcloud-provision))
     ;; Require only the umbrella `sops-secrets` (which itself waits for every
     ;; per-secret decrypt) + the running nextcloud container.  We do NOT name
     ;; the userpw_* secret services: their decls ship COMMENTED OUT in
     ;; lovelace.scm for deploy-safety, and naming a non-existent service makes
     ;; `guix system build` fail (it validates the shepherd requirement graph).
     ;; Pre-activation the script sees the seeds absent and exits 0 (see guard).
     (requirement '(nextcloud sops-secrets))
     (one-shot? #t)
     (respawn? #f)
     (start #~(make-forkexec-constructor
               (list #$%nextcloud-provision-script)
               #:log-file "/var/log/nextcloud-provision.log"))
     (stop #~(make-kill-destructor))
     (documentation
      "Idempotently provision family content in the running NextCloud: deck/groupfolders/memories apps, missing users (Adrian + agents), groups, group folders, calendar/contacts, agent app-passwords.")))))

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

;;; luanti-game-service: Luanti game server with dedicated system user.
;;; Currently used by edison (port 30000 needs to be open in the firewall).
(define luanti-game-service
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
                       ;; Symlink mods from system profile.
                       ;; Always (re)create the symlink so deploys that bump
                       ;; the mod source see the new store path — without
                       ;; this, an old symlink to a stale store path would
                       ;; persist forever.
                       (mkdir-p "/var/lib/luanti/mods")
                       (for-each
                        (lambda (mod-src mod-name)
                          (let ((mod-dir (string-append "/var/lib/luanti/mods/" mod-name)))
                            (false-if-exception (delete-file mod-dir))
                            (symlink mod-src mod-dir)))
                        (list #$(file-append luanti-mobs
                                             "/share/luanti/mods/mobs")
                              #$(file-append luanti-creatura
                                             "/share/luanti/mods/creatura")
                              #$(file-append luanti-mobs-goblins
                                             "/share/luanti/mods/goblins")
                              #$(file-append luanti-mobs-monster
                                             "/share/luanti/mods/mobs_monster")
                              #$(file-append luanti-mobs-skeletons
                                             "/share/luanti/mods/mobs_skeletons")
                              #$(file-append luanti-animalworld
                                             "/share/luanti/mods/animalworld")
                              #$(file-append luanti-draconis
                                             "/share/luanti/mods/draconis")
                              #$(file-append luanti-forgotten-monsters
                                             "/share/luanti/mods/forgotten_monsters")
                              #$(file-append luanti-far-spawn
                                             "/share/luanti/mods/far_spawn"))
                        '("mobs"
                          "creatura"
                          "goblins"
                          "mobs_monster"
                          "mobs_skeletons"
                          "animalworld"
                          "draconis"
                          "forgotten_monsters"
                          "far_spawn"))))

   ;; Luanti server config — managed declaratively at /etc/luanti.conf.
   ;; Tuned for edison: Xeon E5-1650 v4 (6c/12t, 3.6/4.0 GHz boost, 15 G RAM).
   ;; Modest bump above Luanti defaults to use the available headroom — bigger
   ;; active simulation area and longer view distance.  ABM interval and emerge
   ;; thread count are left at their mineclonia/luanti defaults (0.25s ABM,
   ;; auto-scaled emerge thread count which on a 6-core lands around 4-5).
   (simple-service 'luanti-config
                   etc-service-type
                   (list `("luanti.conf"
                           ,(plain-file "luanti.conf"
                                        "# Luanti server configuration — managed by entelequia.

# Identity
server_name = Edison
server_description = Edison Mineclonia server
port = 30000
max_users = 10

# Gameplay
enable_damage = true
creative_mode = false

# Performance (edison: Xeon E5-1650 v4, 6c/12t, 15 G RAM).
# Slight bumps above defaults (4 / 12 / 40) — Luanti's conf parser does
# NOT strip trailing # comments, so values must be on a clean line or
# the whole `5  # default 4` ends up as a non-numeric string and breaks
# mods like Mobs Redo (api.lua does tonumber(settings:get(...)) * 16).
active_block_range = 5
max_block_send_distance = 14
max_simultaneous_block_sends_per_client = 60

# Native Luanti dungeons — mineclonia disables these by default
# (mg_flags ... nodungeons ...) because it generates its own structures.
# But the goblins mod's lair generator REPLACES default Luanti dungeons,
# so without this the mod registers entities but never spawns them.
mcl_enable_mt_dungeons = true

# Goblins lair tuning — make lairs frequent and start near the surface
# so they are findable without deep mining.
goblins_lair_chance = 1
goblins_lair_elev_max = -5
"))))

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
                                     "--config" "/etc/luanti.conf"
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

;;; starbound-game-service: Starbound dedicated multiplayer server (LAN).
;;; Modeled on luanti-game-service, with one crucial difference: Starbound is
;;; proprietary GOG data with NO Guix package, so the server binary + assets
;;; are provisioned out-of-store under /data/starbound — they are NOT in git
;;; or the store.  One-time operator steps before this service can start
;;; (the same patchelf step every GOG game in entelequia/packages/games.scm
;;; documents):
;;;   # copy the whole `game' dir (assets/ mods/ live under it):
;;;   ssh <host> 'tar -C ".../Starbound" -cf - game' | \
;;;     ssh root@edison 'mkdir -p /data/starbound && tar -C /data/starbound -xf -'
;;;   chmod +x /data/starbound/game/linux/starbound_server
;;;   patchelf --set-interpreter \
;;;     "$(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2)" \
;;;     /data/starbound/game/linux/starbound_server
;;; Mutable state (universe, player saves, the generated starbound_server.config)
;;; lives in /data/starbound/storage, owned by the dedicated `starbound' user.
;;; The server's ldd resolves entirely to Guix glibc (no SDL/mesa/GL, not even
;;; libstdc++ — it is headless), so LD_LIBRARY_PATH only needs the game dir for
;;; the dlopen'd, optional libsteam_api.so.  TCP 21025 must be open in edison's
;;; firewall (see edison.scm).
(define starbound-game-service
  (list
   ;; Dedicated system user for the Starbound server
   (simple-service 'starbound-user
                   account-service-type
                   (list (user-account
                          (name "starbound")
                          (comment "Starbound dedicated server")
                          (group "nogroup")
                          (system? #t)
                          (home-directory "/var/lib/starbound")
                          (shell (file-append shadow "/sbin/nologin")))))

   ;; Data dirs + a declarative sbinit.config with ABSOLUTE asset/storage
   ;; paths, so the server is independent of cwd and the install's relative
   ;; bootstrap.  Only the write targets (storage, home) are chowned to
   ;; `starbound'; the rsync'd assets stay root-owned + world-readable, which
   ;; is enough for the server to read them.  sbinit.config is safe to rewrite
   ;; every activation — it is pure config we own and never touches storage/.
   (simple-service 'starbound-dirs
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (for-each
                        (lambda (dir)
                          (mkdir-p dir)
                          (let ((pw (getpwnam "starbound")))
                            (chown dir (passwd:uid pw) (passwd:gid pw))))
                        '("/data/starbound"
                          "/data/starbound/storage"
                          "/var/lib/starbound"))
                       (call-with-output-file "/data/starbound/sbinit.config"
                         (lambda (port)
                           (display "{\n\
  \"assetDirectories\" : [ \"/data/starbound/game/assets\", \"/data/starbound/game/mods\" ],\n\
  \"storageDirectory\" : \"/data/starbound/storage\"\n\
}\n" port)))))

   ;; Starbound shepherd service.  Binary lives at an absolute /data path
   ;; (non-hermetic, provisioned + patchelf'd by the operator — see above),
   ;; not the store.  Binds 0.0.0.0:21025 by default.
   (simple-service 'starbound-server
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Starbound dedicated server")
                     (provision '(starbound))
                     (requirement '(file-systems networking))
                     (start #~(make-forkexec-constructor
                               (list "/data/starbound/game/linux/starbound_server"
                                     "-bootconfig" "/data/starbound/sbinit.config")
                               #:user "starbound"
                               #:group "nogroup"
                               #:directory "/data/starbound/game/linux"
                               ;; Shepherd's own stdout/stderr capture — kept
                               ;; SEPARATE from Starbound's internal log, which
                               ;; the server writes itself at
                               ;; storage/starbound_server.log (storage is owned
                               ;; by `starbound').  Pointing both at the same
                               ;; path makes shepherd pre-create it root-owned,
                               ;; and the unprivileged server then aborts with
                               ;; "Permission denied" opening its own log.
                               #:log-file "/var/log/starbound.log"
                               #:environment-variables
                               (list "HOME=/var/lib/starbound"
                                     "LD_LIBRARY_PATH=/data/starbound/game/linux")))
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
          ;;
          ;; CRITICAL: Do NOT exec into podman directly.  podman exits after
          ;; rm -af completes — leaving this shepherd service in "stopped"
          ;; state.  Shepherd then re-runs us every time a dependent service
          ;; (any container) is started, because dependents require us.
          ;; Each re-run executes `podman rm -af` AGAIN — killing every
          ;; running container.  Same disaster the 8c34e57 fix addressed.
          ;; Keep this process alive after the cleanup with a sleep loop so
          ;; shepherd considers podman-prune "started forever"; further
          ;; dependent starts see the requirement as already satisfied and
          ;; do not re-trigger cleanup.
          (let ((rc (system* #$(file-append coreutils "/bin/timeout")
                             "120"
                             #$(file-append podman "/bin/podman")
                             "rm" "-af")))
            (format #t "podman-prune: rm -af completed with status ~a~%" rc)
            (force-output))
          (let loop () (sleep 86400) (loop))))))

(define podman-prune-service
  (list
   (simple-service 'podman-prune
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (provision '(podman-prune))
                     (requirement '(rootless-podman-shared-root-fs user-processes))
                     ;; Not a one-shot.  The script runs `podman rm -af` once
                     ;; and then sleeps forever — shepherd sees a long-lived
                     ;; "running" service.  This blocks the cleanup from
                     ;; re-running when a dependent container service starts.
                     ;; History: marked one-shot? #t in 1bd8ce1; fixed by
                     ;; sleep-loop in 8c34e57 ("Fix podman-prune re-run
                     ;; killing containers on restart"); the fix was undone
                     ;; in 3b02e7e and added back, then removed again, etc.
                     ;; Keeping the sleep-loop pattern is what works.
                     (respawn? #f)
                     (start #~(make-forkexec-constructor
                               (list #$%podman-prune-script)
                               #:log-file "/var/log/podman-prune.log"))
                     (documentation "Remove all containers from previous boot, then exit."))))))

;;;
;;; make-podman-shepherd-service: build a shepherd-service that runs IMAGE as
;;; podman container NAME, replacing podman's `--replace` flag with explicit
;;; `podman rm -f NAME` pre-cleanup followed by `podman run --rm`.
;;;
;;; Why bypass `oci-container-configuration`?
;;;   `oci-container-execlp` produces `podman run --rm --replace --name X ...`.
;;;   In podman 5.x, when shepherd respawns a container quickly, multiple
;;;   `podman run --replace --name X` invocations end up alive at once.
;;;   They contend for podman's container-name lock; some lose with exit 125
;;;   (and shepherd respawns them — so the leak compounds).  In the wild we
;;;   saw four concurrent `podman run --replace --name habitica-mongo`
;;;   processes after a sops-induced cascade, with `ts-habitica` flapping
;;;   continuously and the netns getting torn down under any container that
;;;   shared it.  ts-netns-watchdog firefights the resulting leaks but does
;;;   not address the cause.
;;;
;;; The fix here is to serialise: the start script does `podman rm -f NAME`
;;; (no-op if absent; idempotent if present) and then `exec podman run --rm`
;;; with no `--replace`.  Two starts can't collide because the first holds the
;;; container-name lock for the whole rm+run sequence, and the second's `rm
;;; -f` simply removes whatever the first finished.  Add a 5 s `respawn-delay`
;;; on top so shepherd doesn't busy-loop on transient failures.
(define* (make-podman-shepherd-service name image
                                        #:key
                                        (requirement '())
                                        (entrypoint #f)
                                        (env '())
                                        (volumes '())
                                        (network #f)
                                        (ports '())
                                        (extra-args '())
                                        (command '())
                                        (respawn-delay 5))
  "Return a shepherd-service that runs IMAGE as rootless podman container
NAME under user 'rafael'.  Volume entries may be plain strings
\"/host/path:/container/path[:opt]\" or file-like objects (e.g. `plain-file`,
`file-append`) that lower to /gnu/store paths concatenated with their mount
target.  See module-level commentary for why this bypasses
oci-container-configuration."
  ;; Capture container name before any record-syntax `(name ...)` field can
  ;; shadow it: Guix's define-record-type* field thunks make sibling field
  ;; values visible within a constructor, so referencing `name` inside e.g.
  ;; `(shepherd-action (name 'foo) (documentation ... name ...))` would
  ;; resolve to the action's name field, not this parameter.
  (let ((container-name name)
        (container-image image))
   (let ((start-script
         (program-file
          (string-append "podman-start-" name)
          #~(begin
              (let* ((pw   (getpwnam "rafael"))
                     (uid  (passwd:uid pw))
                     (gid  (passwd:gid pw))
                     (ruid (string-append "/run/user/"
                                          (number->string uid))))
                (unless (file-exists? ruid)
                  (mkdir ruid)
                  (chown ruid uid gid)
                  (chmod ruid #o700))
                (setenv "XDG_RUNTIME_DIR" ruid)
                (setenv "HOME" (passwd:dir pw))
                ;; /run/setuid-programs first so podman picks up the setuid
                ;; newuidmap/newgidmap wrappers (rootless podman needs them).
                (setenv "PATH"
                        "/run/setuid-programs:/run/current-system/profile/bin")
                (setgid gid)
                (setuid uid)
                ;; Pre-cleanup: forcibly remove any prior container with this
                ;; name.  No-op if absent; serial alternative to --replace.
                ;; `--depend` also removes containers that share this one's
                ;; network namespace (apps/sidecars joined via
                ;; `--network container:NAME`).  Without it a netns owner's
                ;; `rm -f` fails with "has dependent containers" after a reboot
                ;; — the owner never restarts and the whole group flaps
                ;; (feedback_podman_name_flap_after_reboot).  The freed
                ;; dependents are recreated by their own shepherd services,
                ;; which `requirement` orders after this owner.
                ;; Wrapped in `timeout 30` so a wedged podman lock can't
                ;; stall the start indefinitely.
                (system* #$(file-append coreutils "/bin/timeout") "30"
                         #$(file-append podman "/bin/podman")
                         "rm" "-f" "--depend" #$container-name)
                ;; Argument list assembled inline so file-like volume entries
                ;; (e.g. plain-file → /gnu/store path) are properly resolved.
                (apply execlp
                       #$(file-append podman "/bin/podman")
                       "podman"
                       (list "run" "--rm" "--name" #$container-name
                             #$@(append-map
                                 (lambda (e) (list "--env" e))
                                 env)
                             #$@(append-map
                                 (lambda (v) (list "-v" v))
                                 volumes)
                             #$@(append-map
                                 (lambda (p) (list "-p" p))
                                 ports)
                             #$@(if network (list "--network" network) '())
                             #$@(if entrypoint
                                    (list "--entrypoint" entrypoint)
                                    '())
                             #$@extra-args
                             #$container-image
                             #$@command)))))))
    (shepherd-service
     (provision (list (string->symbol container-name)))
     ;; Always require the rootless-podman shepherd services that
     ;; oci-container-configuration would have added implicitly: cgroup
     ;; setup must be in place before any podman run, otherwise the
     ;; container fails to start.
     (requirement (cons* 'cgroups2-fs-owner
                         'cgroups2-limits
                         'rootless-podman-shared-root-fs
                         'user-processes
                         requirement))
     (respawn? #t)
     (respawn-delay respawn-delay)
     ;; shepherd runs the start as root; the script setuids to rafael
     ;; itself (it also has to mkdir /run/user/<uid> on first boot, which
     ;; requires root).  Don't set #:user "rafael" here — that drops privs
     ;; before the script runs, and then setuid/setgid inside the script
     ;; fail with EPERM.
     (start #~(make-forkexec-constructor
               (list #$start-script)
               #:log-file #$(string-append "/var/log/podman-"
                                           container-name ".log")))
     (stop #~(make-kill-destructor))
     (actions
      (list
       (shepherd-action
        (name 'command-line)
        (documentation
         (string-append "Print the start invocation of "
                        container-name "."))
        (procedure
         #~(lambda _
             (format #t "~a~%" #$start-script))))
       (shepherd-action
        (name 'pull)
        (documentation
         (string-append "Run `podman pull " container-image
                        "` as user rafael."))
        (procedure
         #~(lambda _
             (system* #$(file-append podman "/bin/podman")
                      "pull" #$container-image))))))
     (documentation
      (string-append "Podman container " container-name
                     " (serial start; no --replace)"))))))

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
  "Return a shepherd-service for a Tailscale sidecar.
   NAME is the bare service name; the shepherd provision becomes ts-<name>.
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
    (make-podman-shepherd-service
     (string-append "ts-" name)
     "tailscale/tailscale:latest"
     #:requirement '(sops-secrets networking podman-prune)
     #:ports ports
     #:volumes
     (list (string-append ts-state-dir ":/var/lib/tailscale")
           ;; Mount the whole tailscale secrets directory (not just the specific
           ;; file) so the container can start even when GPG decryption is still
           ;; running.  The directory always exists; the key file appears once sops
           ;; finishes.  The entrypoint waits for the file to be non-empty before
           ;; calling containerboot.
           "/run/secrets/tailscale:/run/secrets/tailscale:ro"
           ;; The plain-file lowers to a /gnu/store path; file-append
           ;; concatenates the mount target so the resulting argv element is
           ;; e.g. "/gnu/store/...-ts-serve-habitica.json:/etc/tailscale/...:ro".
           (file-append serve-config-file
                        ":/etc/tailscale/serve-config.json:ro"))
     #:env
     (list "TS_USERSPACE=true"
           "TS_STATE_DIR=/var/lib/tailscale"
           (string-append "TS_AUTHKEY_FILE=/run/secrets/tailscale/"
                          secret-name "_authkey")
           "TS_SERVE_CONFIG=/etc/tailscale/serve-config.json"
           (string-append "TS_HOSTNAME=" name))
     ;; TS_USERSPACE=true uses gVisor netstack (no TUN device) so NET_ADMIN is not
     ;; needed and must be omitted: Podman 5.x passes -t none to pasta when
     ;; NET_ADMIN is present, silently breaking host→container port forwarding.
     #:entrypoint "/bin/sh"
     #:command
     (list "-c"
           (string-append
            "while [ ! -s \"$TS_AUTHKEY_FILE\" ]; do sleep 1; done; "
            "export TS_AUTHKEY=$(cat \"$TS_AUTHKEY_FILE\"); "
            "exec /usr/local/bin/containerboot")))))

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
  "Return a shepherd-service for an app container that shares a Tailscale
sidecar's netns (or runs standalone when SHARE-TS-NETNS? is #f).  PORTS apply
only in standalone mode; in shared mode the sidecar's pasta netns is reused
and ports are exposed via the sidecar instead.  ENTRYPOINT overrides the
image ENTRYPOINT when non-#f; COMMAND overrides the image CMD."
  (make-podman-shepherd-service
   name image
   #:requirement
   (if share-ts-netns?
       (cons* (string->symbol (string-append "ts-" ts-name "-ready"))
              'sops-secrets
              'podman-prune
              requirement)
       ;; Standalone pasta netns: no shared ts sidecar, no secrets consumed,
       ;; so don't depend on sops-secrets — otherwise every respawn churns
       ;; the sops secret chain and cascades.
       (cons 'podman-prune requirement))
   #:entrypoint entrypoint
   #:env environment
   #:volumes volumes
   #:ports (if share-ts-netns? '() ports)
   #:network (and share-ts-netns?
                  (string-append "container:ts-" ts-name))
   #:extra-args extra-arguments
   #:command command))

;;;
;;; heroes-server-game-service: netheroes2 online game server (Heroes of Might &
;;; Magic II), LAN, for rafael + kids.  Rootless podman container running under
;;; user `rafael' (image localhost/heroes-server:0.9.6, provisioned out-of-band
;;; into rafael's podman via `podman load' — the image isn't on any registry).
;;;
;;; Secrets + tunables live in /data/heroes-server/.env (provisioned out-of-band,
;;; NOT in the repo), loaded by the app via dotenv when mounted at the container
;;; WORKDIR.  It must contain ADMIN_PASSWORD (create/host a game), USER_PASSWORD
;;; (join), JWT_KEY, GAME_VERSION=0.28 (netheroes2 client version), plus
;;; GAME_PORT=8090 and API_PORT=3030.
;;;
;;; Ports: API on 3030 (netheroes2 hits <url>/api/...), raw game socket on 8090
;;; (NOT 8080 — that's the ARM web UI).  netheroes2 clients point their server
;;; URL at http://<edison-ip>:3030; the host logs in with ADMIN_PASSWORD to
;;; upload/create the game, joiners with USER_PASSWORD.  Defined here, after
;;; make-app-container, because Guile evaluates top-level defines in order.
(define heroes-server-game-service
  (list
   (simple-service 'heroes-server-dir
                   activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (mkdir-p "/data/heroes-server")
                       ;; Owned by rafael so the rootless (rafael) container can
                       ;; read the out-of-band .env mounted from here.
                       (let ((pw (getpwnam "rafael")))
                         (chown "/data/heroes-server"
                                (passwd:uid pw) (passwd:gid pw)))))

   (simple-service 'heroes-server-container
                   shepherd-root-service-type
                   (list
                    (make-app-container
                     "heroes-server" "localhost/heroes-server:0.9.6"
                     #:share-ts-netns? #f
                     #:ports (list "0.0.0.0:3030:3030" "0.0.0.0:8090:8090")
                     ;; .env is read-WRITE: on first start the app persists
                     ;; generated web-push VAPID keys back into it (a :ro mount
                     ;; makes it crash with EROFS).  Existing keys are preserved.
                     #:volumes
                     (list "/data/heroes-server/.env:/usr/src/app/.env"))))))

;;;
;;; ts-netns-watchdog: keep shared-netns app containers aligned with their
;;; ts-X tailscale sidecars, and clean up leaked `podman run --replace`
;;; processes that pile up under churn.
;;;
;;; Background.  Each app container (e.g. nextcloud, habitica) joins the
;;; netns of its ts-X sidecar via `--network container:ts-X`.  When ts-X is
;;; restarted (`podman run --rm --replace`) its netns is destroyed and the
;;; new ts-X owns a fresh one, but the app's running container is still
;;; pointing at the old (now dead) netns and silently loses connectivity
;;; (HTTP 502 from the tailscale serve proxy, MongooseError "users.findOne
;;; buffering timed out" from habitica → mongo, etc.).  Shepherd doesn't
;;; restart-on-dependency-restart, so the apps stay broken until something
;;; kicks them.
;;;
;;; Separately, podman 5.x's `--replace` races itself when shepherd respawns
;;; tightly: more than one `podman run --replace --name X` can end up alive
;;; for the same name, contending for podman's container-name lock and each
;;; failing with exit 125.  Observed in the wild as four concurrent
;;; `podman run --replace --name habitica-mongo` processes after a
;;; sops-induced cascade.
;;;
;;; The watchdog is a long-running shepherd service that, every 30 s:
;;;   1. For each ts-X in TOPOLOGY, looks for duplicate
;;;      `podman run --replace --name ts-X` (and same for each app); if
;;;      more than one process matches, SIGKILLs all but the newest.
;;;      Shepherd's tracked Main PID is the newest start; the older ones
;;;      are leaks and not part of any healthy lifecycle.
;;;   2. Reads ts-X's current podman container ID and compares against the
;;;      last-seen value cached at /var/lib/ts-netns-watchdog/<name>.  On a
;;;      mismatch, runs `herd restart` for each dependent service so the
;;;      app's `podman run` is re-issued and resolves --network=container:
;;;      ts-X to the new netns.
;;;
;;; Topology is hardcoded below (one entry per ts-X sidecar with its
;;; netns dependents).  When you add a new make-ts-sidecar / make-app-container
;;; pair, register it here too — otherwise the new app silently misses
;;; netns recovery.
;;;
(define %ts-netns-watchdog-script
  (program-file "ts-netns-watchdog"
    #~(begin
        (use-modules (ice-9 popen)
                     (ice-9 rdelim)
                     (ice-9 textual-ports)
                     (srfi srfi-1)
                     (srfi srfi-13))

        ;; (ts-X . (dependent-shepherd-services...)).
        ;; Keep in sync with %app-containers below.
        (define topology
          '(("ts-nextcloud"    . ("nextcloud"))
            ("ts-freshrss"     . ("freshrss"))
            ("ts-wallabag"     . ("wallabag"))
            ("ts-rss-bridge"   . ("rss-bridge"))
            ("ts-searxng"      . ("searxng"))
            ("ts-searxng-kids" . ("searxng-kids"))
            ("ts-grafana"      . ("grafana"))
            ;; edison Mattermost stack: MM + DB + the two shared-netns hermes
            ;; tiers all ride the ts-mattermost sidecar's netns.  hermes-ops is
            ;; NOT here — it is a host-net guix container, not in this netns.
            ("ts-mattermost"   . ("mattermost" "mattermost-db"
                                  "hermes-tutor" "hermes-household"))))

        (define rafael-pw  (getpwnam "rafael"))
        (define rafael-uid (passwd:uid rafael-pw))
        (define rafael-gid (passwd:gid rafael-pw))
        (define rafael-rt  (string-append "/run/user/"
                                          (number->string rafael-uid)))
        (define state-dir  "/var/lib/ts-netns-watchdog")
        (define podman-bin #$(file-append podman "/bin/podman"))
        (define herd-bin   "/run/current-system/profile/bin/herd")
        (define pgrep-bin  #$(file-append procps "/bin/pgrep"))
        (define timeout-bin #$(file-append coreutils "/bin/timeout"))

        (unless (file-exists? state-dir)
          (mkdir state-dir))

        ;; Run COMMAND (a list of strings) as user rafael.  Returns stdout
        ;; with surrounding whitespace stripped, or "" on failure.  We fork
        ;; a child, drop privileges, exec — the parent stays root so it
        ;; can talk to the system shepherd via herd later.
        (define (rafael-stdout command)
          (let* ((p   (pipe))
                 (rd  (car p))
                 (wr  (cdr p))
                 (pid (primitive-fork)))
            (cond
             ((zero? pid)
              (close-port rd)
              (dup2 (port->fdes wr) 1)
              (close-port wr)
              (setenv "XDG_RUNTIME_DIR" rafael-rt)
              (setenv "HOME" (passwd:dir rafael-pw))
              (setenv "PATH"
                      "/run/setuid-programs:/run/current-system/profile/bin")
              (setgid rafael-gid)
              (setuid rafael-uid)
              (apply execlp (car command) command))
             (else
              (close-port wr)
              (let ((out (get-string-all rd)))
                (close-port rd)
                (waitpid pid)
                (string-trim-both (if (eof-object? out) "" out)))))))

        ;; Wrap podman calls with `timeout 10` so a wedged podman daemon
        ;; (lock contention from leaked --replace processes) can't stall
        ;; the whole sweep.  Cleanup must run before the query so the
        ;; query has a chance to succeed.
        (define (container-id name)
          (rafael-stdout
           (list timeout-bin "10"
                 podman-bin "ps" "--filter"
                 (string-append "name=^" name "$")
                 "--format" "{{.ID}}")))

        (define (read-state name)
          (let ((path (string-append state-dir "/" name)))
            (if (file-exists? path)
                (string-trim-both (call-with-input-file path get-string-all))
                "")))

        (define (write-state name id)
          (let ((path (string-append state-dir "/" name)))
            (call-with-output-file path
              (lambda (port) (display id port)))))

        ;; List PIDs (oldest first) of `podman run --replace --name $NAME`
        ;; processes owned by rafael.  Uses `pgrep -o`/`-n` ordering by
        ;; calling pgrep without -o so we get all matches; sort by stat-time.
        (define (podman-run-pids name)
          (let* ((out (rafael-stdout
                      (list pgrep-bin "-u" "rafael" "-f"
                            (string-append
                             "podman run( --[a-z-]+)+ --name " name "( |$)"))))
                 (lines (filter (lambda (s) (not (string-null? s)))
                                (string-split out #\newline))))
            ;; pgrep emits one PID per line; not stat-time sorted, but the
            ;; numeric PID is monotonic on this kernel since boot, so
            ;; sorting numerically gives oldest-first.
            (sort (map string->number lines) <)))

        ;; SIGKILL all but the newest matching `podman run --replace --name
        ;; $NAME` process.  Newest PID = highest numeric PID under our
        ;; assumption of monotonic PID assignment since boot.
        (define (cleanup-leaked-runs name)
          (let ((pids (podman-run-pids name)))
            (when (> (length pids) 1)
              (let ((stale (drop-right pids 1)))
                (format #t "ts-netns-watchdog: ~a has ~a leaked podman runs (PIDs ~a); killing stale~%"
                        name (length pids) pids)
                (for-each
                 (lambda (pid)
                   (false-if-exception (kill pid SIGKILL)))
                 stale)))))

        (define (herd-restart-dep dep)
          (format #t "ts-netns-watchdog: herd restart ~a~%" dep)
          (system* herd-bin "restart" dep))

        ;; Main loop — run forever, sleeping 30s between sweeps.
        ;; first-run: record current state without triggering restarts (boot).
        (let loop ((first-run? #t))
          ;; Pass 1: leaked-process cleanup.  Must run before any podman
          ;; query, since a wedged --replace lock will stall podman ps.
          (for-each
           (lambda (entry)
             (let ((ts-name (car entry))
                   (deps    (cdr entry)))
               (cleanup-leaked-runs ts-name)
               (for-each cleanup-leaked-runs deps)))
           topology)
          ;; Pass 2: netns-change detection.  Now that locks are clean,
          ;; podman ps should respond promptly (10s timeout regardless).
          (for-each
           (lambda (entry)
             (let* ((ts-name    (car entry))
                    (deps       (cdr entry))
                    (current-id (container-id ts-name))
                    (last-id    (read-state ts-name)))
               (unless (string-null? current-id)
                 (cond
                  (first-run?
                   (write-state ts-name current-id))
                  ((string-null? last-id)
                   (write-state ts-name current-id))
                  ((not (string=? current-id last-id))
                   (format #t
                           "ts-netns-watchdog: ~a netns changed (~a -> ~a); restarting ~a~%"
                           ts-name last-id current-id deps)
                   (write-state ts-name current-id)
                   (for-each herd-restart-dep deps))))))
           topology)
          (force-output)
          (sleep 30)
          (loop #f)))))

(define ts-netns-watchdog-service
  (simple-service
   'ts-netns-watchdog
   shepherd-root-service-type
   (list
    (shepherd-service
     (provision '(ts-netns-watchdog))
     (requirement '(rootless-podman-shared-root-fs user-processes))
     (respawn? #t)
     (start #~(make-forkexec-constructor
               (list #$%ts-netns-watchdog-script)
               #:log-file "/var/log/ts-netns-watchdog.log"))
     (stop #~(make-kill-destructor))
     (documentation
      "Recover shared-netns app containers when their ts-X sidecar is replaced.")))))

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

   ;; ── SearxNG (adult + kids, both via Mullvad) ─────────────────────────
   ;; Both SearxNG instances share gluetun-pihole's Mullvad netns so search-
   ;; engine queries leave through Mullvad while the inbound tailnet path
   ;; stays plain Tailscale.  Reuses pihole's existing WG key/device — no
   ;; extra Mullvad device, no extra sops secret.
   ;;
   ;;   client ──TS──► ts-searxng ──proxy──► host.containers.internal:8080
   ;;                                          │
   ;;                                  [gluetun-pihole netns]
   ;;                                          ├─ pihole        :53, :80
   ;;                                          ├─ searxng       :8080
   ;;                                          ├─ searxng-kids  :8081
   ;;                                          └─ wg0 → Mullvad → upstream engines
   ;;
   ;; Tradeoff: a gluetun-pihole restart bounces searxng too.  Acceptable
   ;; because pihole's gluetun is the most stable of the three.
   ;;
   ;; The kids instance shares the same Tailscale auth key as the adult
   ;; (#:secret-name "searxng") — must be REUSABLE in Tailscale admin.
   ;; Both SearxNG containers also share the same secret_key (HMAC-only).
   (make-podman-shepherd-service
    "searxng" "searxng/searxng:latest"
    #:requirement '(gluetun-pihole sops-secrets)
    #:volumes
    (list "/data/searxng:/etc/searxng:rw"
          "/run/secrets/searxng/secret_key:/run/secrets/secret_key:ro")
    #:env (list "SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml")
    #:network "container:gluetun-pihole"
    #:extra-args
    (list "--cap-drop=ALL" "--cap-add=CHOWN" "--cap-add=SETGID" "--cap-add=SETUID"))

   (make-podman-shepherd-service
    "searxng-kids" "searxng/searxng:latest"
    #:requirement '(gluetun-pihole sops-secrets)
    #:volumes
    (list "/data/searxng-kids:/etc/searxng:rw"
          "/run/secrets/searxng/secret_key:/run/secrets/secret_key:ro")
    ;; The image's ENV sets GRANIAN_PORT=8080.  searx.webapp:app is loaded
    ;; by granian directly, so SEARXNG_PORT (only honoured by the dev-mode
    ;; searx.webapp:run() entrypoint) has no effect — override GRANIAN_PORT.
    #:env (list "SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml"
                "GRANIAN_PORT=8081")
    #:network "container:gluetun-pihole"
    #:extra-args
    (list "--cap-drop=ALL" "--cap-add=CHOWN" "--cap-add=SETGID" "--cap-add=SETUID"))

   ;; serve-port refers to the host-side port that gluetun-pihole publishes
   ;; (8090/8091, see ports list above), not the in-container 8080/8081.
   (make-ts-sidecar "searxng" #:serve-port 8090
                    #:backend-host "host.containers.internal")
   (make-ts-sidecar "searxng-kids" #:serve-port 8091 #:secret-name "searxng"
                    #:backend-host "host.containers.internal")))

;;;
;;; VPN-routed containers (Pi-hole + qBittorrent via Gluetun/Mullvad)
;;;

(define %vpn-containers
  (list
   ;; ── Pi-hole ───────────────────────────────────────────────────────────
   ;; Gluetun creates a VPN network namespace; Pi-hole and Tailscale sidecar
   ;; connect to it differently: pihole shares gluetun's netns, ts-pihole
   ;; is separate (proxies to the published port).
   (make-podman-shepherd-service
    "gluetun-pihole" "qmcgaw/gluetun:latest"
    #:requirement '(networking)
    #:volumes
    (list "/data/gluetun-pihole:/gluetun"
          "/run/secrets/mullvad/pihole_wg_private_key:/run/secrets/wg-key:ro"
          "/run/secrets/mullvad/pihole_wg_address:/run/secrets/wg-address:ro")
    #:env
    (list "VPN_SERVICE_PROVIDER=mullvad"
          "VPN_TYPE=wireguard"
          ;; Pin exit to Nordics — close to Oslo for low latency on pihole's
          ;; DNS path and on searxng's outbound search-engine queries; also
          ;; reduces 403/anti-bot rates from engines like Wikidata that
          ;; aggressively rate-limit US Mullvad ranges.
          "SERVER_COUNTRIES=Norway,Sweden,Denmark")
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); export WIREGUARD_ADDRESSES=$(cat /run/secrets/wg-address); exec /gluetun-entrypoint")
    ;; 8090/8091 also published here because searxng + searxng-kids share
    ;; gluetun-pihole's netns to route their outbound through Mullvad (see
    ;; SearxNG block below).  Host ports 8090/8091 (not 8080/8081) avoid
    ;; collision with gluetun-qbt which already publishes :8080 for qbt's
    ;; webUI; the in-netns ports searxng listens on are still 8080/8081.
    #:ports (list "53:53/tcp" "53:53/udp" "0.0.0.0:8053:80"
                  "0.0.0.0:8090:8080" "0.0.0.0:8091:8081")
    #:extra-args (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun"))

   (make-podman-shepherd-service
    "pihole" "pihole/pihole:latest"
    #:requirement '(gluetun-pihole)
    #:volumes
    (list "/data/pihole/etc:/etc/pihole"
          "/data/pihole/dnsmasq:/etc/dnsmasq.d"
          "/run/secrets/pihole/webpassword:/run/secrets/webpassword:ro")
    #:env
    (list "WEBPASSWORD_FILE=webpassword"
          "TZ=Europe/Oslo"
          "DNSMASQ_LISTENING=all"
          "FTLCONF_webserver_serve_all=true")
    #:network "container:gluetun-pihole")

   (make-ts-sidecar "pihole" #:serve-port 8053
                    #:backend-host "host.containers.internal"
                    #:ts-state-dir "/data/tailscale/pihole")

   ;; ── qBittorrent ───────────────────────────────────────────────────────
   (make-podman-shepherd-service
    "gluetun-qbt" "qmcgaw/gluetun:latest"
    #:requirement '(networking)
    #:volumes
    (list "/data/gluetun-qbt:/gluetun"
          "/run/secrets/mullvad/qbt_wg_private_key:/run/secrets/wg-key:ro"
          "/run/secrets/mullvad/qbt_wg_address:/run/secrets/wg-address:ro")
    #:env
    (list "VPN_SERVICE_PROVIDER=mullvad"
          "VPN_TYPE=wireguard")
    #:entrypoint "/bin/sh"
    #:command (list "-c"
                    "export WIREGUARD_PRIVATE_KEY=$(cat /run/secrets/wg-key); export WIREGUARD_ADDRESSES=$(cat /run/secrets/wg-address); exec /gluetun-entrypoint")
    #:ports (list "0.0.0.0:8080:8080")
    #:extra-args (list "--cap-add=NET_ADMIN" "--device=/dev/net/tun"))

   (make-podman-shepherd-service
    "qbittorrent" "lscr.io/linuxserver/qbittorrent:latest"
    #:requirement '(gluetun-qbt)
    #:volumes
    (list "/data/qbittorrent/config:/config"
          "/data/qbittorrent/downloads:/downloads")
    #:env (list "PUID=1000" "PGID=1000" "TZ=Europe/Oslo" "WEBUI_PORT=8080")
    #:network "container:gluetun-qbt")

   (make-ts-sidecar "qbt" #:serve-port 8080
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
   ;; NOTE: this previously ran as rootful podman (user "root"
   ;; container-user "root").  The new make-podman-shepherd-service runs all
   ;; containers as user rafael (rootless), which is incompatible with
   ;; --privileged + block-device access.  Until that's reconciled, expect
   ;; smartctl-exporter to fail to read SMART data; downgrade gracefully or
   ;; revisit by giving rafael CAP_SYS_RAWIO via a separate mechanism.
   (make-podman-shepherd-service
    "smartctl-exporter" "prometheuscommunity/smartctl-exporter:latest"
    #:requirement '(networking)
    #:network "host"
    #:extra-args (list "--privileged"))

   ;; ── Prometheus ────────────────────────────────────────────────────────
   ;; Prometheus runs with host networking so it can scrape native services
   ;; (node-exporter :9100, smartctl-exporter :9633) on the host directly.
   ;; ts-prometheus is NOT used for prometheus itself — instead prometheus
   ;; listens on the host at :9090, and grafana reaches it at host:9090.
   (make-ts-sidecar "prometheus" #:serve-port 9090
                    #:backend-host "host.containers.internal")
   (make-podman-shepherd-service
    "prometheus" "prom/prometheus:latest"
    #:requirement '(sops-secrets networking cgroups2-fs-owner cgroups2-limits
                    rootless-podman-shared-root-fs user-processes)
    #:network "host"
    #:volumes
    (list "/data/prometheus:/prometheus"
          (file-append %prometheus-config
                       ":/etc/prometheus/prometheus.yml:ro"))
    ;; Run as container root (= host rafael uid 1000) so it can write to
    ;; /data/prometheus, which is owned by rafael.
    #:extra-args '("--user=0")
    #:command
    (list "--config.file=/etc/prometheus/prometheus.yml"
          "--storage.tsdb.path=/prometheus"
          "--web.listen-address=:9090"))

   ;; ── Grafana ───────────────────────────────────────────────────────────
   (make-ts-sidecar "grafana" #:serve-port 3000)
   (make-app-container
    "grafana" "grafana/grafana:latest"
    #:volumes
    (list "/data/grafana:/var/lib/grafana"
          "/run/secrets/grafana/admin_password:/run/secrets/grafana-admin-pw:ro"
          (file-append %grafana-prometheus-datasource
                       ":/etc/grafana/provisioning/datasources/prometheus.yaml:ro"))
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

;;; lovelace-container-services: register all rootless-podman containers as
;;; native shepherd-services.  We bypass `oci-service-type` because its
;;; `podman run --rm --replace` produces races under rapid respawn (see the
;;; commentary on `make-podman-shepherd-service`).  All container-producing
;;; helpers (`make-ts-sidecar`, `make-app-container`, plus the inline
;;; `make-podman-shepherd-service` calls in %vpn-containers / %monitoring-
;;; containers) now return raw `shepherd-service` records.
;;; Note: rootless-podman-service-type MUST still be in the system services
;;; list separately.  It provides cgroup group creation, subids for rafael,
;;; and the cgroups2-* / rootless-podman-shared-root-fs shepherd services
;;; that our containers depend on.
(define lovelace-container-services
  (append
   ;; Gate services: one-shot readiness checks that ensure each ts-* sidecar
   ;; container is registered in podman before the app container tries
   ;; --network=container:ts-<name>.  Without these, app containers race
   ;; against their sidecar's `podman run` and fail with "no container found".
   (map make-ts-ready-service
        '("freshrss" "nextcloud" "wallabag" "rss-bridge" "searxng"
          "searxng-kids" "grafana"))
   ;; All shepherd-services (containers + sidecars) registered in one batch.
   (list
    (simple-service 'lovelace-podman-containers
                    shepherd-root-service-type
                    (append %app-containers
                            %vpn-containers
                            %monitoring-containers)))))
