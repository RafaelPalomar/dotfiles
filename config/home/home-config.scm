(define-module (config home home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu services)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages version-control)
  #:use-module (config home services emacs)
  #:use-module (config packages emacs-evil-snipe))

(home-environment
 (packages (list emacs
                 emacs-avy
                 emacs-beacon
                 emacs-evil
                 emacs-evil-collection
                 emacs-evil-commentary
                 emacs-evil-paredit
                 emacs-evil-snipe
                 emacs-evil-surround
                 emacs-general
                 emacs-helpful
                 emacs-modus-themes
                 emacs-paredit
                 emacs-projectile
                 font-iosevka-aile
                 font-jetbrains-mono
                 fontconfig
                 git
                 weechat))

 (services (list (service home-emacs-config-service-type)
                 (service home-dotfiles-service-type
                          (home-dotfiles-configuration
			                     (directories '("../../files")))))))
