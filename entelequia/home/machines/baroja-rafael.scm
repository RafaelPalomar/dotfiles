(define-module (entelequia home machines baroja-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; baroja home environment — rafael
;;;
;;; Lenovo ThinkPad X220, Intel iGPU.  bspwm desktop + laptop services
;;; (batsignal).  Modelled on hopper-rafael but WITHOUT the gaming profile
;;; — the Sandy Bridge HD 3000 isn't a gaming target.  No networking
;;; profile (no personal tailscaled/VPN userspace here).  Add profiles
;;; later as baroja's role firms up.

(home-environment
 (packages
  (append (base-home-packages #:gpu-type 'intel)
          (development-home-packages)
          email-home-packages
          documentation-home-packages))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (laptop-home-services)
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
