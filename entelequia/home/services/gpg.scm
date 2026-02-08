(define-module (entelequia home services gpg)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:export (home-gpg-service-type))

;;; GPG home service
;;;
;;; Consolidated GPG agent configuration extracted from systems/desktop.scm.
;;; Uses pinentry-rofi for graphical password prompts, with SSH support.

(define home-gpg-service-type
  (service-type (name 'home-gpg)
                (description "GPG agent configuration with rofi pinentry and SSH support")
                (extensions
                 (list (service-extension
                        home-gpg-agent-service-type
                        (lambda (config)
                          (home-gpg-agent-configuration
                           (pinentry-program
                            (file-append pinentry-rofi "/bin/pinentry-rofi"))
                           (ssh-support? #t)
                           (default-cache-ttl 28800)      ; 8 hours
                           (max-cache-ttl 28800)          ; 8 hours
                           (default-cache-ttl-ssh 28800)  ; 8 hours
                           (max-cache-ttl-ssh 28800)))))) ; 8 hours
                (default-value #f)))
