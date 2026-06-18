(define-module (entelequia home services scratch-launcher)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (scratch-launcher-home-service))

;;; Scratch launcher (home service)
;;;
;;; Drops an XDG desktop entry at ~/.local/share/applications/scratch.desktop
;;; so "Scratch" appears in rofi.  sxhkd launches rofi as `rofi -show drun`,
;;; which reads .desktop files from XDG application dirs (including
;;; ~/.local/share/applications) — so this entry is offered as an app the
;;; child can pick to open the Scratch web editor in Librewolf.
;;;
;;; Pairs with:
;;;   - the librewolf-policy Cookies.Allow for scratch.mit.edu (so login and
;;;     saved projects survive Librewolf's clear-on-shutdown), and
;;;   - librewolf being on the kid's PATH via the desktop home profile.
;;;
;;; Reusable across kid users (adrian@hopper, leandro@alucard); wire it into
;;; the relevant home/machines/<host>-<user>.scm services list.

(define %scratch-desktop
  (plain-file "scratch.desktop"
   "[Desktop Entry]
Type=Application
Version=1.0
Name=Scratch
GenericName=Programming for kids
Comment=Make games, animations and stories at scratch.mit.edu
Exec=librewolf --new-window https://scratch.mit.edu
Icon=applications-education
Terminal=false
Categories=Education;Development;
Keywords=scratch;programming;games;code;kids;
StartupNotify=true
"))

(define scratch-launcher-home-service
  (simple-service 'scratch-launcher
                  home-files-service-type
                  (list (list ".local/share/applications/scratch.desktop"
                              %scratch-desktop))))
