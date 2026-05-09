(define-module (entelequia home machines lovelace-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles server)
  #:use-module (entelequia home services server-suite)
  #:use-module (gnu home))

;;; lovelace home environment — rafael
;;;
;;; Headless home server.  Server-suite only.

(home-environment
 (packages
  (append (base-home-packages)
          (server-home-packages)))
 (services (server-home-services)))
