(define-module (entelequia home machines hopper-adrian)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles python-learning)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; hopper home environment — adrian
;;;
;;; Minimal desktop env (bspwm + dotfiles + games + python-learning).  No
;;; dev / email / docs.  Filter out age-inappropriate and broken titles:
;;;   - they-are-billions: graphic content
;;;   - caves-of-qud-native: Mesa 25.2.3 + Unity 2021 black-screen on Intel
;;;     UHD 620 (same bug as curie's gfx1150).  Wine variant works.

(home-environment
 (packages
  (append (base-home-packages)
          (python-learning-home-packages)
          (gaming-home-packages
           #:exclude '("they-are-billions"
                       "caves-of-qud-native"))))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (laptop-home-services)
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
