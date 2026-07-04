(define-module (entelequia home machines alucard-leandro)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles python-learning)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services librewolf-kids)
  #:use-module (entelequia home services archimedes)
  #:use-module (gnu packages games)        ; fheroes2 (HoMM2 engine)
  #:use-module (entelequia packages games) ; netheroes2 (online HoMM2 fork)
  #:use-module (guix gexp)                 ; plain-file (netheroes2 .desktop)
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
  (append (base-home-packages #:gpu-type 'nvidia)
          (python-learning-home-packages)
          (gaming-home-packages
           #:exclude '("caves-of-qud"))
          ;; Heroes of Might & Magic II: fheroes2 (single-player / hot-seat) +
          ;; netheroes2 (online multiplayer, heroes2.online).  Both need the HoMM2
          ;; assets in leandro's ~/.local/share/fheroes2 to run.
          (list fheroes2
                netheroes2)))
 (services
  (append
   (common-home-services #:nvidia? #t)
   (desktop-home-services)
   ;; netheroes2 app-menu entry (the package strips the fork's own .desktop to
   ;; avoid colliding with fheroes2, which ships its own).  Same as curie.
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
              "Comment=Online multiplayer HoMM II via heroes2.online "
              "(fheroes2 fork; uses the fheroes2 assets)\n"
              "Exec=netheroes2\n"
              "Icon=fheroes2\n"
              "Terminal=false\n"
              "Categories=Game;StrategyGame;\n"
              "Keywords=homm;heroes;might;magic;online;multiplayer;\n"))))))
   (list librewolf-kids-home-service
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles/common")))))
   (archimedes-home-service #:learner "leandro"))))
