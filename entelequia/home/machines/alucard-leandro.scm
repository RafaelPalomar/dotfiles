(define-module (entelequia home machines alucard-leandro)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles python-learning)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services librewolf-kids)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; alucard home environment — leandro
;;;
;;; Same bspwm desktop setup as rafael (NVIDIA — no gfx1150 black-screen
;;; issue, so native CoQ works).  No dev / email / docs profiles.

(home-environment
 (packages
  (append (base-home-packages)
          (python-learning-home-packages)
          (gaming-home-packages
           #:exclude '("caves-of-qud"))))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (list librewolf-kids-home-service
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
