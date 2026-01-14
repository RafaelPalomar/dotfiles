(define-module (config home home-config)
  #:use-module (config home services emacs)
  #:use-module (config packages mutt-oauth2)
  #:use-module (config packages cyrus-sasl-xoauth2)
  #:use-module (config packages emacs-denote-silo)
  #:use-module (config packages emacs-persp-projectile)
  #:use-module (config packages emacs-tabspaces)
  #:use-module (config packages emacs-ob-mermaid)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages guile-xyz)
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

(define %emacs-packages
  (list emacs-ace-window
        emacs-all-the-icons
        emacs-all-the-icons-dired
        emacs-avy
        emacs-beacon
        emacs-cmake-mode
        emacs-counsel
        emacs-counsel-projectile
        emacs-dashboard
        emacs-denote
        emacs-denote-silo
        emacs-dockerfile-mode
        emacs-embark
        emacs-evil
        emacs-evil-collection
        emacs-evil-commentary
        emacs-evil-goggles
        emacs-evil-paredit
        emacs-evil-surround
        emacs-evil-matchit
        emacs-evil-org
        emacs-forge
        emacs-geiser
        emacs-geiser-guile
        emacs-general
        emacs-gptel
        emacs-helpful
        emacs-ivy
        emacs-ivy-rich
        emacs-lua-mode
        emacs-magit
        emacs-markdown-mode
        emacs-markdown-preview-mode
        emacs-mixed-pitch
        emacs-modus-themes
        emacs-mu4e-dashboard
        emacs-nano-theme
        emacs-nano-modeline
        emacs-nerd-icons
        emacs-ob-mermaid
        emacs-orderless
        emacs-org-appear
        emacs-org-mime
        emacs-org-modern
        emacs-org-reveal
        emacs-paredit
        emacs-perspective
        emacs-persp-projectile
        emacs-pgtk
        emacs-projectile
        emacs-pyvenv
        emacs-rainbow-delimiters
        emacs-restart-emacs
        emacs-rg
        emacs-svg-lib
        emacs-swiper
        emacs-tabspaces
        emacs-undo-fu
        emacs-use-package
        emacs-visual-fill-column
        emacs-which-key
        emacs-yaml
        emacs-yaml-mode
        ))

(home-environment
 (packages (append %emacs-packages
                   %email-packages
                   (list font-cica
                         font-abattis-cantarell
                         font-fira-code
                         font-google-roboto
                         font-inconsolata
                         font-iosevka-aile
                         font-jetbrains-mono
                         fontconfig
                         git
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
                         texlive
                         texlive-minted
                         texlive-xetex
                         weechat
                         )))

 (services (list (service home-emacs-config-service-type)
                 ;; Drop files (gnu stow replacement)
                 (service home-dotfiles-service-type
                          (home-dotfiles-configuration
                           (directories '("../../files"))))

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
                                   ("LITERAL_VALUE" . ,(literal-string "${abc}"))))


                 ;; Bashrc
                 (service home-bash-service-type
                          (home-bash-configuration
                           (aliases '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/ntnu.no --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                                      ("auth-email-uio" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/uio.no --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no")
                                      ("mbsync-all" . "guix shell cyrus-sasl-xoauth2 -L ~/dotfiles -- mbsync -a")
                                      )))))))
