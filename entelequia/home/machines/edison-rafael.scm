(define-module (entelequia home machines edison-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles server)
  #:use-module (entelequia home services server-suite)
  #:use-module (entelequia home services poppins)
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
 (services
  (append (server-home-services)
          ;; Mary Poppins — the household agent (personal domain) on the family
          ;; server.  Authenticates to the family NextCloud as mary-poppins;
          ;; needs the app-pw in sops at /run/secrets/nextcloud/poppins-apppw +
          ;; a personal PKS root (~/pks-personal).  Until those exist it's just
          ;; an inert `poppins' wrapper on PATH (no work data, ever).
          (poppins-home-service))))
