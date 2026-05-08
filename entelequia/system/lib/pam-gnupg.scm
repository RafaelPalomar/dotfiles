(define-module (entelequia system lib pam-gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (pam-gnupg-service-type))

;;; pam-gnupg: forward the SLiM/login password into gpg-agent at session
;;; start so anything that decrypts via the user's GPG key (pass,
;;; pass-secret-service → Nextcloud OAuth token, mu4e, git signing)
;;; succeeds silently for the rest of the session — no pinentry prompts.
;;;
;;; Prerequisites (deployed declaratively elsewhere or set once by hand):
;;;   1. ~/.pam-gnupg listing the keygrips to preset.  Deployed via
;;;      home-dotfiles-service-type from dotfiles/.pam-gnupg.
;;;   2. allow-preset-passphrase in gpg-agent.conf — set via the
;;;      home-gpg-agent-configuration extra-content field.
;;;   3. The user's GPG passphrase MUST equal their login password.
;;;      Set once by hand: `gpg --passwd <KEYID>`.  Mismatched keygrips
;;;      are silently skipped — pinentry would still fire if/when those
;;;      keys are needed.
;;;
;;; Modeled on (gnu services pam-mount).

(define (pam-gnupg-pam-service _)
  (define pam-gnupg-entry
    (pam-entry
     (control "optional")
     (module (file-append pam-gnupg "/lib/security/pam_gnupg.so"))))
  (list
   (pam-extension
    (transformer
     (lambda (pam)
       (if (member (pam-service-name pam)
                   '("slim" "login" "greetd" "gdm-password" "sddm"))
           (pam-service
            (inherit pam)
            (auth (append (pam-service-auth pam)
                          (list pam-gnupg-entry)))
            (session (append (pam-service-session pam)
                             (list pam-gnupg-entry))))
           pam))))))

(define pam-gnupg-service-type
  (service-type
   (name 'pam-gnupg)
   (extensions
    (list (service-extension pam-root-service-type pam-gnupg-pam-service)))
   (default-value #f)
   (description "Forward the SLiM/login password into gpg-agent via
pam_gnupg, presetting passphrases for the keygrips listed in
@file{~/.pam-gnupg}.")))
