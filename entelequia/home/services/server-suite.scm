(define-module (entelequia home services server-suite)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:export (server-home-services))

;;; Server-suite home services
;;;
;;; Replaces the legacy entelequia/systems/server.scm.  Minimal home
;;; environment for headless boxes (edison, lovelace) — no desktop services,
;;; no emacs daemon, no GPG pinentry-rofi.  Just shell + env config plus the
;;; sys-/home- alias pair so reconfigures are one short keystroke.

(define (server-home-services)
  (list
   ;; Bash configuration
   (service home-bash-service-type
            (home-bash-configuration
             (aliases
              '(("ll"  . "ls -lah --color=auto")
                ("la"  . "ls -A --color=auto")
                ("ls"  . "ls --color=auto")
                ("gs"  . "git status")
                ("gl"  . "git log --oneline -10")
                ("sys-reconfigure"  . "sudo guix time-machine -C ~/.dotfiles/channels-lock.scm -- system reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/system/machines/$(hostname).scm")
                ("sys-update"       . "git -C ~/.dotfiles pull && sudo guix time-machine -C ~/.dotfiles/channels-lock.scm -- system reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/system/machines/$(hostname).scm")
                ("home-reconfigure" . "guix time-machine -C ~/.dotfiles/channels-lock.scm -- home reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/home/machines/$(hostname)-$(whoami).scm")
                ("home-update"      . "git -C ~/.dotfiles pull && guix time-machine -C ~/.dotfiles/channels-lock.scm -- home reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/home/machines/$(hostname)-$(whoami).scm")))
             (bashrc
              (list (plain-file "bashrc-server"
                                "# Server-specific shell config
export EDITOR=vim
export PAGER=less
export LESS='-R'

# Coloured prompt showing hostname
PS1='\\[\\033[1;32m\\]\\u@\\h\\[\\033[0m\\]:\\[\\033[1;34m\\]\\w\\[\\033[0m\\]\\$ '

# direnv hook
command -v direnv >/dev/null && eval \"$(direnv hook bash)\"
")))))

   ;; Environment variables
   (simple-service 'server-env-vars
                   home-environment-variables-service-type
                   '(("EDITOR" . "vim")
                     ("VISUAL" . "vim")))))
