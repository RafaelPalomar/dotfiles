(define-module (entelequia home machines hopper-adrian)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles python-learning)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services librewolf-kids)
  #:use-module (entelequia home services scratch-launcher)
  #:use-module (entelequia home services archimedes)
  #:use-module (gnu packages games)        ; fheroes2 (HoMM2 engine)
  #:use-module (entelequia packages games) ; netheroes2 (online HoMM2 fork)
  #:use-module (guix gexp)                 ; plain-file (netheroes2 .desktop)
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
;;;   - no-mans-sky: tried but unplayable on UHD 620 (well below the iGPU
;;;     minimum); switched adrian to starbound, which the chassis handles
;;;     comfortably.  Game files + wine prefix wiped from disk too.

(home-environment
 (packages
  (append (base-home-packages #:gpu-type 'intel)
          (python-learning-home-packages)
          (gaming-home-packages
           #:exclude '("they-are-billions"
                       "caves-of-qud-native"
                       "no-mans-sky"))
          ;; Heroes of Might & Magic II: fheroes2 (single-player / hot-seat) +
          ;; netheroes2 (online multiplayer via edison heroes-server).  Both need
          ;; the HoMM2 assets in adrian's ~/.local/share/fheroes2 to run.
          (list fheroes2
                netheroes2)))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (laptop-home-services)
   ;; netheroes2 app-menu entry (the package strips the fork's own .desktop to
   ;; avoid colliding with fheroes2, which ships its own).  Same as curie/leandro.
   (list
    (simple-service
     'netheroes2-desktop-launcher
     home-files-service-type
     (list
      (list ".local/share/applications/netheroes2.desktop"
            (plain-file "netheroes2.desktop"
             (string-append
              "[Desktop Entry]\n"
              "Version=1.0\n"
              "Type=Application\n"
              "Name=Heroes II Online (netheroes2)\n"
              "GenericName=Turn-based strategy\n"
              "Comment=Online multiplayer HoMM II via the edison heroes-server "
              "(fheroes2 fork; uses the fheroes2 assets)\n"
              "Exec=netheroes2\n"
              "Icon=fheroes2\n"
              "Terminal=false\n"
              "Categories=Game;StrategyGame;\n"
              "Keywords=homm;heroes;might;magic;online;multiplayer;\n"))))))
   (list librewolf-kids-home-service
         scratch-launcher-home-service
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles/common")))))
   (archimedes-home-service #:learner "adrian"))))
