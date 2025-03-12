(define-module (config home home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module (config home services emacs))
;;  #:use-module (config packages emacs-evil-snipe))

(define %emacs-packages
  (list emacs-ace-window
        emacs-avy
        emacs-beacon
        emacs-cmake-mode
        emacs-consult
        emacs-evil
        emacs-evil-collection
        emacs-evil-commentary
        emacs-evil-paredit
        ;; emacs-evil-snipe
        emacs-evil-surround
        emacs-geiser
        emacs-geiser-guile
        emacs-general
        emacs-helpful
        emacs-magit
        emacs-modus-themes
        emacs-paredit
        emacs-perspective
        emacs-projectile
        emacs-undo-fu
        ))

(home-environment
 (packages (append %emacs-packages
                   (list emacs-pgtk
                         font-iosevka-aile
                         font-jetbrains-mono
                         fontconfig
                         git
                         ripgrep
                         weechat)))

 (services (list (service home-emacs-config-service-type)
                 ;; Drop files (gnu stow replacement)
                 (service home-dotfiles-service-type
                          (home-dotfiles-configuration
                           (directories '("../../files"))))
                 ;; Bashrc
                 (service home-bash-service-type
                          (home-bash-configuration
                           (environment-variables '(("PS1" . "\\[\\e[1;32m\\]\\u \\[\\e[1;34m\\]\\w \\[\\e[0m\\]λ ")))
                           (aliases '(("gemacs" .
                                       (string-append "guix " "shell " "emacs-pgtk "
                                                     (string-append "emacs-ace-window "
                                                                     "emacs-avy "
                                                                     "emacs-beacon "
                                                                     "emacs-cmake-mode "
                                                                     "emacs-consult "
                                                                     "emacs-evil "
                                                                     "emacs-evil-collection "
                                                                     "emacs-evil-commentary "
                                                                     "emacs-evil-paredit "
                                                                     ;; "emacs-evil-snipe "
                                                                     "emacs-evil-surround "
                                                                     "emacs-geiser "
                                                                     "emacs-geiser-guile "
                                                                     "emacs-general "
                                                                     "emacs-helpful "
                                                                     "emacs-modus-themes "
                                                                     "emacs-magit "
                                                                     "emacs-paredit "
                                                                     "emacs-perspective "
                                                                     "emacs-projectile "
                                                                     "emacs-undo-fu "
								     "ripgrep ")
                                                      "-- "
                                                      "emacs")))))))))
