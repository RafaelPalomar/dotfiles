(define-module (entelequia home home-config)
  #:use-module (entelequia home services emacs)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia packages mutt-oauth2)
  #:use-module (entelequia packages cyrus-sasl-xoauth2)
  #:use-module (entelequia packages emacs)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp))

(define %email-packages
  (list cyrus-sasl-xoauth2
        isync
        msmtp))

(home-environment
 (packages (append
                   %email-packages
                   documentation-home-packages
                   (list bash-completion
                         deskflow
                         font-cica
                         font-abattis-cantarell
                         font-fira-code
                         font-google-roboto
                         font-inconsolata
                         font-iosevka-aile
                         font-jetbrains-mono
                         fontconfig
                         git
                         glibc
                         glibc-locales
                         gnutls
                         guile-3.0
                         guile-gcrypt
                         guile-git
                         ;;guile-libyaml
                         guile-chickadee
                         guile-webutils
                         guile-parted
                         guile-newt
                         libgit2-glib
                         librewolf
                         mu
                         mutt-oauth2
                         pinentry-gnome3
                         nextcloud-client
                         nyacc
                         password-store
                         python-pygments
                         ripgrep
                         texlive-scheme-full
                         texlive-minted
                         texlive-xetex
                         ungoogled-chromium
                         weechat
                         )))

 (services
  (list (service home-emacs-config-service-type)
        ;; Drop files (gnu stow replacement)
        (service home-dotfiles-service-type
                 (home-dotfiles-configuration
                  (directories '("../../dotfiles"))))

        (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (ssh-support? #t)
                  (pinentry-program (file-append pinentry-gnome3 "/bin/pinentry-gnome3"))
                  (default-cache-ttl 3600)
                  (max-cache-ttl 86400)))

        ;; (simple-service 'gpg-agent-env
        ;;                 home-environment-variables-service-type
        ;;                 `(("GPG_TTY" . "$TTY")
        ;;                   ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh")))


        (simple-service 'some-useful-env-vars-service
                        home-environment-variables-service-type
                        `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                          ("USELESS_VAR" . #f)
                          ("_JAVA_AWT_WM_NONREPARENTING" . #t)
                          ("LITERAL_VALUE" . ,(literal-string "${abc}"))
                          ("TERMINAL" . "kitty")))

        (service home-dbus-service-type)

        (service home-pipewire-service-type
                 (home-pipewire-configuration (enable-pulseaudio? #t)))

        ;; Bashrc
        (service home-bash-service-type
                 (home-bash-configuration
                  (aliases '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/ntnu.no --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                             ("auth-email-uio" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/uio.no --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no")
                             ("mbsync-all" . "guix shell cyrus-sasl-xoauth2 -L ~/dotfiles -- mbsync -a")
                             )))))))
