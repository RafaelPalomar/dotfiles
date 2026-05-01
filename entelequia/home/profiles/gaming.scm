(define-module (entelequia home profiles gaming)
  #:use-module (entelequia packages games)
  #:use-module (gnu packages linux)       ; For dualsensectl
  #:use-module (gnu packages luanti)
  #:use-module (gnu packages emulators)   ; For scummvm
  #:use-module (gnu packages compression) ; For innoextract
  #:use-module (gnu packages wine)        ; For wine64-staging
  #:use-module (nongnu packages gog)      ; For lgogdownloader
  #:export (gaming-home-packages))

;;; Adding a new game:
;;;   1. gog-install /path/to/game.sh
;;;   2. patchelf --set-interpreter $(readlink -f /run/current-system/profile/lib/ld-linux-x86-64.so.2) GameBinary
;;;   3. ldd GameBinary | grep "not found" → choose tier, add to games.scm
;;;   4. Add to list below + guix home reconfigure

;;; Gaming home profile
;;;
;;; GOG game launchers deployed as Guix packages.
;;; Each launcher is a shell wrapper that sets LD_LIBRARY_PATH to
;;; Guix store paths, refreshed automatically on 'guix home reconfigure'.
;;;
;;; Adding a new game:
;;;   1. gog-install /path/to/game.sh          (one-time)
;;;   2. Add entry to entelequia/packages/games.scm
;;;   3. Add to gaming-home-packages below
;;;   4. guix home reconfigure

(define* (gaming-home-packages #:key (exclude '()))
  "Return the gaming home profile package list.

EXCLUDE is a list of package-name strings to omit (e.g. for child
profiles that shouldn't ship age-inappropriate titles)."
  (filter
   (lambda (pkg)
     (not (member ((@ (guix packages) package-name) pkg) exclude)))
   (list gog-crypt-of-the-necrodancer
         gog-death-road-to-canada
         gog-duskers
         gog-papers-please
         gog-terraria
         gog-wizard-of-legend
         gog-slay-the-spire
         gog-torchlight-2
         gog-they-are-billions
         gog-9-kings
         gog-he-is-coming
         gog-gobliiins
         gog-gobliins-2
         gog-goblins-quest-3
         scummvm
         innoextract
         wine64-staging
         coq-caves-of-qud
         coq-caves-of-qud-native
         bay12-dwarf-fortress
         luanti-mineclonia-csm
         luanti-halon
         luanti-mobs-goblins
         ;; Minetest Game only (depend on 'default' mod, incompatible with Mineclonia/VoxeLibre):
         ;; luanti-mobs
         ;; luanti-mobs-animal
         ;; luanti-mobs-monster
         ;; luanti-moreores
         ;; luanti-unifieddyes
         dualsensectl
         lgogdownloader)))
