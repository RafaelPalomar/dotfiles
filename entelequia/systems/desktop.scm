(define-module (entelequia systems desktop)
  #:use-module (entelequia home services emacs)
  #:use-module (entelequia home services desktop)
  #:use-module (entelequia home services encrypted-usb)
  #:use-module (entelequia home services containers)
  #:use-module (entelequia packages polybar-themes)
  #:use-module (gnu packages package-management)
  #:use-module (entelequia packages fonts)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:export (desktop-home-services))

(use-service-modules desktop mcron pm syncthing)

(define desktop-home-services
  (list
   ;; Set environment variables for every session

   ;; Place other files (simple-service 'profile-files-service
   ;;                 home-files-service-type (list `(".inputrc" ,(local-file "../files/inputrc"))))

   ;; GnuPG configuration
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-rofi "/bin/pinentry-rofi"))
             (ssh-support? #t)
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))

   ;; Emacs configuration
   (service home-emacs-config-service-type)

   ;; Run user dbus session
   (service home-dbus-service-type)

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; Container configuration (podman and distrobox)
   (service home-containers-service-type)

   (service home-pipewire-service-type
            (home-pipewire-configuration (enable-pulseaudio? #t)))

   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories '("../../dotfiles"))))

   (simple-service 'polybar-themes
                   home-files-service-type
                   `((".config/polybar"
                      ,(directory-union "polybar-themes"
                                        (list polybar-themes)))))

   (simple-service 'slicer-profile-setup
                   home-bash-service-type
                   (home-bash-extension
                    (bash-profile
                     (list (plain-file "setup-slicer-profile-qt5"
                                       "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-5 5")
                           (plain-file "setup-slicer-profile-qt6"
                                       "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-6 6")))))
   (service home-bash-service-type
            (home-bash-configuration
             (aliases '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/ntnu.no --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                        ("auth-email-uio" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/uio.no --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no")
                        ("mbsync-all" . "guix shell cyrus-sasl-xoauth2 -L ~/dotfiles -- mbsync -a")))
             (bashrc (list (plain-file "bashrc-direnv"
                                       "# if direnv is installed, run the hook
                                                                   if hash direnv 2> /dev/null; then
                                                                       # get the shell name
                                                                       tmp_shell=\"$(basename \"$SHELL\")\"
                                                                       # add the hook
                                                                       eval \"$(direnv hook ${tmp_shell})\"
                                                                   fi")
                           (plain-file "bashrc-container-isolation"
                                       "# Source container isolation script if in a distrobox container
if [ -n \"$CONTAINER_ID\" ] && [ -f /etc/profile.d/zz-container-guix-isolation.sh ]; then
    . /etc/profile.d/zz-container-guix-isolation.sh
fi")))))
   (simple-service 'slicer-env-vars
                   home-environment-variables-service-type
                   `(("PATH" . "$HOME/.local/bin:$PATH")
                     ("SLICER_GUIX_PROFILE" . "$HOME/.slicer-guix-profile-6")))


   ;; Start background jobs (service home-mcron-service-type
   ;;          (home-mcron-configuration
   ;;           (jobs (list
   ;;             #~(job
   ;;                '(next-hour (range 0 24 4)) "~/.dotfiles/.bin/sync-passwords")))))

   ;; File synchronization (service home-syncthing-service-type)

   ;; Monitor battery levels
   (service home-batsignal-service-type)

   ;; DataLocker Sentry ONE auto-unlock
   ;; Note: No auto-lock on disconnect - use keyboard shortcut or polybar button
   (service home-encrypted-usb-service-type
            (encrypted-usb-configuration
             (name "datalocker")
             (vendor-id "230a")
             (model-id "1550")
             (mount-point "UNLOCKER")
             (unlock-command "$HOME/.local/bin/datalocker-unlock")
             (lock-command #f)  ; Manual lock only
             (poll-interval 2)))

   ;; Udiskie for auto-mounting devices (service home-udiskie-service-type)
   ))
