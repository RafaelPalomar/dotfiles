(define-module (entelequia home machines hopper-rafael)
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

;;; hopper home environment — rafael
;;;
;;; Work laptop (Dell XPS 13, Intel iGPU).  Full bspwm desktop + laptop
;;; services (batsignal).  No networking profile (work-box doesn't run the
;;; personal tailscaled / VPN userspace bits that curie does).

(home-environment
 (packages
  (append (base-home-packages #:gpu-type 'intel)
          (development-home-packages)
          (gaming-home-packages)
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
