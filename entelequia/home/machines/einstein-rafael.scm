(define-module (entelequia home machines einstein-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles role)
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
  (append (base-home-packages #:gpu-type 'nvidia)
          (development-home-packages)
          (networking-home-packages)
          (home-role-packages 'work)
          documentation-home-packages))
 (services
  (append
   (common-home-services #:nvidia? #t
                         #:email-aliases? #t #:slicer-aliases? #t #:claude-skills? #t)
   (desktop-home-services)
   (chromium-home-services)
   ;; Work role: ~/.config/entelequia/role marker only (no work tailnet —
   ;; curie-only; no #:manage-mail? — blanket .mbsyncrc still serves it).
   (home-role-services 'work)
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles/common"
                                  "../../../dotfiles/rafael"))))))))
