(define-module (entelequia home profiles server)
  #:use-module (entelequia lib helpers)
  #:export (server-home-packages))

;;; Minimal server home packages
;;;
;;; Only essential CLI tools for interactive server sessions.
;;; No desktop, GUI, or development packages.

(define (server-home-packages)
  (specifications->packages
   '("tmux"
     "htop"
     "btop"
     "ncdu"
     "jq"
     "git"
     "vim"
     "direnv"
     "ripgrep"
     "tree")))
