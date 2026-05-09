(define-module (entelequia home machines alucard-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; alucard home environment — rafael
;;;
;;; Shared desktop, NVIDIA GPU.  Full bspwm desktop with games (alucard is
;;; the gaming box).

(home-environment
 (packages
  (append (base-home-packages)
          (development-home-packages)
          (gaming-home-packages)
          email-home-packages
          documentation-home-packages))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
