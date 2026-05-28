(define-module (entelequia home machines einstein-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles networking)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services chromium)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; einstein home environment — rafael
;;;
;;; Desktop, NVIDIA GPU.  Full bspwm desktop, no laptop services.
;;; No gaming profile (einstein is the work/dev box).

(home-environment
 (packages
  (append (base-home-packages)
          (development-home-packages)
          (networking-home-packages)
          email-home-packages
          documentation-home-packages))
 (services
  (append
   (common-home-services #:nvidia? #t)
   (desktop-home-services)
   (chromium-home-services)
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
