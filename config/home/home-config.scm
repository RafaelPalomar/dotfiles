(define-module (config home home-config)
  #:use-module (config home services emacs)
  #:use-module (config packages mutt-oauth2)
  #:use-module (config packages cyrus-sasl-xoauth2)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control))
;;  #:use-module (config packages emacs-evil-snipe))

(define %email-packages
  (list cyrus-sasl-xoauth2
        isync
        msmtp))

(define %emacs-packages
  (list emacs-ace-window
        emacs-all-the-icons-dired
        emacs-avy
        emacs-beacon
        emacs-cmake-mode
        emacs-consult
        emacs-dashboard
        emacs-denote
        emacs-embark
        emacs-evil
        emacs-evil-collection
        emacs-evil-commentary
        emacs-evil-goggles
        emacs-evil-paredit
        ;; emacs-evil-snipe
        emacs-evil-surround
        emacs-geiser
        emacs-geiser-guile
        emacs-general
        emacs-gptel
        emacs-helpful
        emacs-magit
        emacs-marginalia
        emacs-markdown-mode
        emacs-modus-themes
        emacs-nano-theme
        emacs-nano-modeline
        emacs-nerd-icons
        emacs-mu4e-dashboard
        emacs-orderless
        emacs-org-superstar
        emacs-paredit
        emacs-perspective
        emacs-pgtk
        emacs-projectile
        emacs-rg
        emacs-svg-lib
        emacs-undo-fu
        emacs-yaml
        emacs-yaml-mode
        ))

(home-environment
 (packages (append %emacs-packages
                   %email-packages
                   (list font-fira-code
                         font-google-roboto
                         font-inconsolata
                         font-iosevka-aile
                         font-jetbrains-mono
                         fontconfig
                         git
                         guile-3.0
                         guile-git
                         libgit2-glib
                         mu
                         mutt-oauth2
                         password-store
                         ripgrep
                         weechat
                         )))

 (services (list (service home-emacs-config-service-type)
                 ;; Drop files (gnu stow replacement)
                 (service home-dotfiles-service-type
                          (home-dotfiles-configuration
                           (directories '("../../files"))))
                 ;; Bashrc
                 (service home-bash-service-type
                          (home-bash-configuration
                           (aliases '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/ntnu.no --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                                      ("auth-email-uio" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/uio.no --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no")
                                      ("mbsync-all" . "guix shell cyrus-sasl-xoauth2 -L ~/dotfiles -- mbsync -a")
                                      )))))))
