(define-module (entelequia home services poppins)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (alpha-agent poppins)                       ; poppins-launcher (pinned channel)
  #:use-module (alpha-agent manifests poppins)             ; poppins-tool-profile
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
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

(define (poppins-wrapper key-file nc-apppw-file personal-root nc-user nc-calendar nc-url)
  "An executable `poppins' that injects the OpenRouter key + the NextCloud
app-password (read from sops-decrypted files) and the household env, then execs
the poppins launcher with its tool profile on GUIX_ENVIRONMENT."
  (program-file
   "poppins"
   #~(begin
       (use-modules (ice-9 rdelim))
       (define (read-secret f)
         (and (file-exists? f)
              (call-with-input-file f
                (lambda (p) (let ((s (read-line p))) (and (string? s) s))))))
       (let ((k  (read-secret #$key-file))
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

(define (poppins-cli key-file nc-apppw-file personal-root nc-user nc-calendar nc-url)
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
               (copy-file #$(poppins-wrapper key-file nc-apppw-file personal-root
                                             nc-user nc-calendar nc-url)
                          (string-append #$output "/bin/poppins"))
               (chmod (string-append #$output "/bin/poppins") #o755))))
    (synopsis "Mary Poppins launcher wrapper (injects OpenRouter + NextCloud creds)")
    (description "Wrapper that injects the OpenRouter key and the mary-poppins
NextCloud app-password (from sops-decrypted files) plus the household/memory env,
then execs the poppins launcher.")
    (home-page "https://github.com/RafaelPalomar/alpha-agent")
    (license license:gpl3+)))

(define* (poppins-home-service
          #:key (key-file "/run/secrets/openrouter/rafael")
                (nc-apppw-file "/run/secrets/nextcloud/poppins-apppw")
                (personal-root "/home/rafael/pks-personal")
                (nc-user "mary-poppins")
                (nc-calendar "family_shared_by_mary-poppins")
                (nc-url "https://nextcloud.drake-karat.ts.net"))
  "Deploy `poppins' (pi + the Mary Poppins wrapper) into the home profile."
  (list
   (simple-service 'poppins
                   home-profile-service-type
                   (list pi (poppins-cli key-file nc-apppw-file personal-root
                                         nc-user nc-calendar nc-url)))))
