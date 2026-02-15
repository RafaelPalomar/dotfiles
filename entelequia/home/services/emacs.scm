(define-module (entelequia home services emacs)
  #:use-module (entelequia packages emacs)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:export (%emacs-packages
            home-emacs-config-service-type))

(use-package-modules emacs emacs-xyz fonts tex cmake)

;;; Emacs home service
;;;
;;; Consolidated Emacs service with daemon, 60+ packages,
;;; and auto-restart on failure. This is the single source
;;; of truth for Emacs configuration (replaces the old
;;; home/services/emacs.scm and home-services/emacs.scm).

(define %emacs-packages
  (list emacs-ace-window
        emacs-aider
        emacs-all-the-icons
        emacs-all-the-icons-dired
        emacs-avy
        emacs-beacon
        emacs-cmake-mode
        emacs-counsel
        emacs-counsel-projectile
        emacs-company
        emacs-copilot
        emacs-dashboard
        emacs-denote
        emacs-denote-silo
        emacs-diredfl
        emacs-dirvish
        emacs-dockerfile-mode
        emacs-doom-themes
        emacs-embark
        emacs-envrc
        emacs-evil
        emacs-evil-collection
        emacs-evil-commentary
        emacs-evil-escape
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
        emacs-guix
        emacs-helpful
        emacs-nerd-icons
        emacs-hl-todo
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
        emacs-org-super-agenda
        emacs-paredit
        emacs-perspective
        emacs-persp-projectile
        emacs-prescient
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
        emacs-ws-butler
        emacs-yaml
        emacs-yaml-mode))

(define (home-emacs-config-profile-service config)
  (append (list emacs-lucid
                font-fira-code
                font-abattis-cantarell)
          %emacs-packages))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "A service for configuring Emacs with daemon and auto-restart")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)
                       (service-extension
                        home-shepherd-service-type
                        (lambda (config)
                          (list (shepherd-service
                                 (provision '(emacs))
                                 (start #~(make-forkexec-constructor
                                           (list #$(file-append emacs-lucid "/bin/emacs")
                                                 "--fg-daemon")
                                           #:environment-variables
                                           (cons* (string-append "DISPLAY=" (or (getenv "DISPLAY") ":0"))
                                                  (string-append "XAUTHORITY="
                                                                 (or (getenv "XAUTHORITY")
                                                                     (string-append (getenv "HOME") "/.Xauthority")))
                                                  (default-environment-variables))))
                                 (stop #~(make-kill-destructor))
                                 (respawn? #t)
                                 (auto-start? #t)
                                 (documentation "Emacs daemon with auto-restart on failure")))))))
                (default-value #f)))
