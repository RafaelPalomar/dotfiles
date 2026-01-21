(define-module (entelequia systems desktop)
  #:use-module (entelequia home-services emacs)
  #:use-module (entelequia home-services desktop)
  ;;  #:use-module (daviwil home-services udiskie)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:export (desktop-home-services))

(use-service-modules desktop mcron pm syncthing)

(define desktop-home-services (list
                               ;; Set environment variables for every session

                               ;; Place other files (simple-service 'profile-files-service
                               ;;                 home-files-service-type (list `(".inputrc" ,(local-file "../files/inputrc"))))

                               ;; GnuPG configuration
                               (service home-gpg-agent-service-type (home-gpg-agent-configuration
                                                                     (pinentry-program
                                                                      (file-append pinentry-emacs "/bin/pinentry-emacs")) (ssh-support? #t) (default-cache-ttl 28800) (max-cache-ttl
                                                                                                                                                                       28800) (default-cache-ttl-ssh 28800) (max-cache-ttl-ssh 28800)))

                               ;; Emacs configuration
                               (service home-emacs-config-service-type)

                               ;; Run user dbus session
                               (service home-dbus-service-type)

                               ;; Set up desktop environment
                               (service home-desktop-service-type)

                               (service home-dotfiles-service-type
	                                      (home-dotfiles-configuration
	                                       (directories '("../../.files"))))

                               ;; Start background jobs (service home-mcron-service-type
                               ;;          (home-mcron-configuration
                               ;;           (jobs (list
                               ;;             #~(job
                               ;;                '(next-hour (range 0 24 4)) "~/.dotfiles/.bin/sync-passwords")))))

                               ;; File synchronization (service home-syncthing-service-type)

                               ;; Monitor battery levels
                               (service home-batsignal-service-type)

                               ;; Udiskie for auto-mounting devices (service home-udiskie-service-type)
                               ))
