(define-module (config home home-config)
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
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module (config home services emacs))
;;  #:use-module (config packages emacs-evil-snipe))

(define %email-packages
  (list isync))

(define %emacs-packages
  (list emacs-ace-window
        emacs-all-the-icons-dired
        emacs-avy
        emacs-beacon
        emacs-cmake-mode
        emacs-consult
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
        emacs-orderless
        emacs-paredit
        emacs-perspective
        emacs-projectile
        emacs-rg
        emacs-undo-fu
        emacs-yaml
        emacs-yaml-mode
        ))

(home-environment
 (packages (append %emacs-packages
                   %email-packages
                   (list emacs-pgtk
                         font-iosevka-aile
                         font-jetbrains-mono
                         fontconfig
                         git
                         guile-3.0
                         guile-git
                         libgit2-glib
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
                                       (string-append "guix "
                                                      "shell "
                                                      "emacs-pgtk "
                                                      "guile "
                                                      "guile-git "
                                                      "libgit2-glib "
                                                      "ripgrep "
                                                      (string-append "emacs-ace-window "
                                                                     "emacs-all-the-icons-dired "
                                                                     "emacs-avy "
                                                                     "emacs-beacon "
                                                                     "emacs-cmake-mode "
                                                                     "emacs-consult "
                                                                     "emacs-denote "
                                                                     "emacs-embark "
                                                                     "emacs-evil "
                                                                     "emacs-evil-collection "
                                                                     "emacs-evil-commentary "
                                                                     "emacs-evil-goggles "
                                                                     "emacs-evil-paredit "
                                                                     ;; "emacs-evil-snipe "
                                                                     "emacs-evil-surround "
                                                                     "emacs-geiser "
                                                                     "emacs-geiser-guile "
                                                                     "emacs-general "
                                                                     "emacs-gptel "
                                                                     "emacs-helpful "
                                                                     "emacs-magit "
                                                                     "emacs-marginalia "
                                                                     "emacs-markdown-mode "
                                                                     "emacs-modus-themes "
                                                                     "emacs-orderless "
                                                                     "emacs-paredit "
                                                                     "emacs-perspective "
                                                                     "emacs-projectile "
                                                                     "emacs-rg "
                                                                     "emacs-undo-fu "
                                                                     "emacs-yaml "
                                                                     "emacs-yaml-mode "
                                                                     "ripgrep ")
                                                      "-- "
                                                      "emacs")))))))))
