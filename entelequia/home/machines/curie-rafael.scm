(define-module (entelequia home machines curie-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles networking)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services chromium)
  #:use-module (entelequia home services tailscale-work)
  #:use-module (btv tailscale)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; curie home environment — rafael
;;;
;;; Laptop, AMD GPU.  Full bspwm desktop + laptop services (batsignal) +
;;; userspace tailscaled for the work tailnet (side-by-side with the
;;; system-level personal tailscaled).

(home-environment
 (packages
  (append (base-home-packages #:gpu-type 'amd)
          (development-home-packages)
          (networking-home-packages)
          email-home-packages
          documentation-home-packages
          (gaming-home-packages)
          (list tailscaled)))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (laptop-home-services)
   (chromium-home-services)
   (list (service home-tailscale-work-service-type)
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
