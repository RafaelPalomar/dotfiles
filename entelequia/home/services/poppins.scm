(define-module (entelequia home services poppins)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)                             ; profile, packages->manifest
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (alpha-agent poppins)                       ; poppins-launcher (pinned channel)
  #:use-module (alpha-agent manifests poppins)             ; poppins-tool-profile
  #:use-module (alpha-agent poppins-bridge)                ; poppins-bridge (Mattermost daemon)
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:use-module (gnu home services shepherd)                ; home-shepherd-service-type
  #:use-module (gnu services shepherd)                     ; shepherd-service, forkexec
  #:use-module (gnu packages bash)                         ; bash-minimal (/bin/sh)
  #:export (poppins-home-service))

;;; poppins — Mary Poppins, the household agent (PERSONAL domain).
;;;
;;; Belongs on the PERSONAL side (the family server / personal box), NOT the
;;; work fleet: its memory is the personal PKS root and it authenticates to the
;;; family NextCloud as `mary-poppins'.  The wrapper injects, at launch, the
;;; secrets the L1 container scrubs:
;;;   OPENROUTER_API_KEY <- KEY-FILE          (pi auth; ideally a poppins-specific key)
;;;   NC_APPPW           <- NC-APPPW-FILE      (the mary-poppins NextCloud app-password)
;;; and sets the non-secret NextCloud + memory env (NC_USER / NC_CALENDAR /
;;; NC_URL / DENOTECLI_DIRS).  poppins.scm's sandbox preserves all of these.
;;;
;;; System prerequisite (on the deploy host): sops-secrets for KEY-FILE and
;;; NC-APPPW-FILE, owner = the running user, mode 0400.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

(define (poppins-wrapper openrouter-env-file nc-apppw-file personal-root nc-user nc-calendar nc-url)
  "An executable `poppins' that injects the OpenRouter key + the NextCloud
app-password and the household env, then execs the poppins launcher with its
tool profile on GUIX_ENVIRONMENT.  The wrapper (running as rafael at launch)
reads the secrets from sops-decrypted files — the agent process never sees them.
OPENROUTER_API_KEY is REUSED from the household tier's dotenv env-file
(/run/secrets/hermes-household/env) during the Hermes->colony migration; at
Hermes teardown (P3) point this at Poppins's own key-file instead."
  (program-file
   "poppins"
   #~(begin
       (use-modules (ice-9 rdelim) (srfi srfi-13))
       (define (read-secret f)
         (and (file-exists? f)
              (call-with-input-file f
                (lambda (p) (let ((s (read-line p))) (and (string? s) s))))))
       ;; Extract VAR=value from a dotenv file (KEY=VALUE lines), stripping
       ;; surrounding quotes — reuses the household OpenRouter key at runtime.
       (define (read-dotenv f var)
         (and (file-exists? f)
              (call-with-input-file f
                (lambda (p)
                  (let ((pfx (string-append var "=")))
                    (let loop ()
                      (let ((line (read-line p)))
                        (cond ((eof-object? line) #f)
                              ((string-prefix? pfx line)
                               (let* ((v (substring line (string-length pfx)))
                                      (n (string-length v)))
                                 (if (and (>= n 2) (memv (string-ref v 0) '(#\" #\')))
                                     (substring v 1 (- n 1))
                                     v)))
                              (else (loop))))))))))
       (let ((k  (read-dotenv #$openrouter-env-file "OPENROUTER_API_KEY"))
             (np (read-secret #$nc-apppw-file)))
         (when k  (setenv "OPENROUTER_API_KEY" k))
         (when np (setenv "NC_APPPW" np)))
       ;; Personal-domain memory root (matches poppins.scm's sandbox share) and
       ;; the family NextCloud surface.  No work data anywhere here.
       (setenv "DENOTECLI_DIRS" #$personal-root)
       (setenv "NC_USER"     #$nc-user)
       (setenv "NC_CALENDAR" #$nc-calendar)
       (setenv "NC_URL"      #$nc-url)
       (unless (getenv "GUIX_ENVIRONMENT")
         (setenv "GUIX_ENVIRONMENT" #$poppins-tool-profile))
       (apply execl #$(file-append poppins-launcher "/bin/poppins")
              "poppins" (cdr (command-line))))))

(define (poppins-cli openrouter-env-file nc-apppw-file personal-root nc-user nc-calendar nc-url)
  (package
    (name "poppins-cli")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(poppins-wrapper openrouter-env-file nc-apppw-file personal-root
                                             nc-user nc-calendar nc-url)
                          (string-append #$output "/bin/poppins"))
               (chmod (string-append #$output "/bin/poppins") #o755))))
    (synopsis "Mary Poppins launcher wrapper (injects OpenRouter + NextCloud creds)")
    (description "Wrapper that injects the OpenRouter key and the mary-poppins
NextCloud app-password (from sops-decrypted files) plus the household/memory env,
then execs the poppins launcher.")
    (home-page "https://github.com/RafaelPalomar/alpha-agent")
    (license license:gpl3+)))

;;; --- the Mattermost bridge (chat surface) ----------------------------------
;;;
;;; `poppins-bridge' logs in to the family Mattermost as the `ms-poppins' bot and
;;; shells out to the `poppins' wrapper per message.  It runs on the HOST (this
;;; home shepherd), NOT inside the agent's L1 sandbox: it is transport only.  Its
;;; Mattermost coordinates (URL / bot token / allowed channel) are REUSED from the
;;; household tier's provisioned fragment — the same `ms-poppins' identity the old
;;; Hermes household container used — so taking over the chat surface is just a
;;; matter of which process holds the connection.  Run the bridge alongside, then
;;; stop the old hermes-household container, and the new Poppins answers #household.
;;;
;;; Python deps (requests + websocket-client) resolve via the bridge profile's
;;; etc/profile (GUIX_PYTHONPATH); `poppins' resolves from the home profile.

(define %mm-fragment "/var/lib/mattermost-provision/hermes-household.env")

(define poppins-bridge-profile
  ;; A standalone profile of just the bridge; its etc/profile exports a complete
  ;; GUIX_PYTHONPATH (requests + websocket-client are propagated).
  (profile (content (packages->manifest (list poppins-bridge)))))

(define (poppins-bridge-start-script mm-fragment)
  (mixed-text-file "poppins-bridge-start"
    ;; NB: -e only, NOT -u: the guix profile's etc/profile (sourced below)
    ;; expands $GUIX_PYTHONPATH unguarded, which aborts under `set -u'.
    "#!/bin/sh\nset -e\n"
    "FRAG=\"" mm-fragment "\"\n"
    ;; Wait (respawn) until the provisioner has written the bot token fragment.
    "[ -f \"$FRAG\" ] || { echo 'poppins-bridge: waiting for MM fragment' >&2; sleep 10; exit 1; }\n"
    ;; MATTERMOST_URL / MATTERMOST_TOKEN / MATTERMOST_ALLOWED_CHANNELS (+ _USERS,
    ;; which the bridge ignores) — the same ms-poppins coordinates Hermes used.
    "set -a\n. \"$FRAG\"\nset +a\n"
    ;; python3 + GUIX_PYTHONPATH for requests/websocket-client.
    ". " (file-append poppins-bridge-profile "/etc/profile") "\n"
    ;; The `poppins' wrapper lives in the home profile.
    "export PATH=\"$HOME/.guix-home/profile/bin:$PATH\"\n"
    "exec " (file-append poppins-bridge-profile "/bin/poppins-bridge") "\n"))

(define (poppins-bridge-shepherd-service mm-fragment)
  (list
   (shepherd-service
    (documentation "Mary Poppins Mattermost bridge (ms-poppins bot -> poppins -p)")
    (provision '(poppins-bridge))
    (start #~(make-forkexec-constructor
              (list #$(file-append bash-minimal "/bin/sh")
                    #$(poppins-bridge-start-script mm-fragment))
              #:environment-variables
              (list (string-append "HOME=" (getenv "HOME"))
                    (string-append "PATH=" (getenv "HOME")
                                   "/.guix-home/profile/bin:/run/current-system/profile/bin"))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (string-append (getenv "HOME") "/.local/state"))
                          "/poppins-bridge.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define* (poppins-home-service
          #:key (openrouter-env-file "/run/secrets/hermes-household/env")
                (nc-apppw-file "/run/secrets/nextcloud/poppins-apppw")
                (personal-root "/home/rafael/pks-personal")
                (nc-user "mary-poppins")
                (nc-calendar "family_shared_by_mary-poppins")
                (nc-url "https://nextcloud.drake-karat.ts.net")
                (mm-fragment %mm-fragment))
  "Deploy `poppins' (pi + the Mary Poppins wrapper) into the home profile, plus
the Mattermost bridge daemon that exposes her on the family `#household' channel."
  (list
   (simple-service 'poppins
                   home-profile-service-type
                   (list pi (poppins-cli openrouter-env-file nc-apppw-file personal-root
                                         nc-user nc-calendar nc-url)))
   (simple-service 'poppins-bridge
                   home-shepherd-service-type
                   (poppins-bridge-shepherd-service mm-fragment))))
