(define-module (entelequia home machines edison-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles server)
  #:use-module (entelequia home services server-suite)
  #:use-module (gnu home))

;;; edison home environment — rafael
;;;
;;; Headless multimedia server.  Server-suite only: bash, env vars, no
;;; desktop.  No dotfiles service — server homes run with the minimal
;;; bashrc snippet shipped by server-suite.scm.

(home-environment
 (packages
  (append (base-home-packages)
          (server-home-packages)))
 (services (server-home-services)))
