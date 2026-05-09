(define-module (entelequia home services chromium)
  #:use-module (gnu)
  #:use-module (gnu packages chromium)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (%chromium-rafael-profiles
            chromium-home-packages
            chromium-home-services))

;;; Native ungoogled-chromium home service
;;;
;;; Replaces the Flatpak `io.github.ungoogled_software.ungoogled_chromium'
;;; with the Guix `ungoogled-chromium' package and ships per-profile
;;; .desktop launchers that each use a dedicated `--user-data-dir' directory
;;; under ~/.config/.  Each profile gets its own preferences, cookies, and
;;; (per-profile) extension installs.
;;;
;;; The default search engine policy is NOT set here — it is a mandatory
;;; per-machine policy dropped into /etc/chromium/policies/managed/ by the
;;; system-side service `chromium-policy-service'
;;; (entelequia/system/lib/chromium-policy.scm).  Chromium reads
;;; /etc/chromium/policies/{managed,recommended}/ on Linux regardless of
;;; --user-data-dir, so a single system policy applies to all four
;;; launchers.
;;;
;;; Bitwarden install — manual, once per profile
;;; --------------------------------------------
;;; ungoogled-chromium has no working force-install path
;;; (ExtensionInstallForcelist is upstream-rejected: chromium-web-store
;;; only re-enables MANUAL Web Store browsing, not the background
;;; install poller).  Each of the four profiles therefore needs a
;;; one-time setup:
;;;
;;;   1. Open the launcher (e.g. "Chromium (NTNU)").
;;;   2. Install chromium-web-store: download the latest .crx from
;;;        https://github.com/NeverDecaf/chromium-web-store/releases
;;;      then chrome://extensions → Developer Mode ON → drag-drop the .crx.
;;;   3. Visit https://chromewebstore.google.com/detail/bitwarden/
;;;      nngceckbapebfimnlniiiahkandclblb
;;;      and click "Add to Chromium" — chromium-web-store handles the
;;;      install prompt.
;;;   4. Repeat for the other three profiles (each --user-data-dir is
;;;      fully independent, so extensions don't carry over).

(define %chromium-rafael-profiles
  ;; Each entry: (slug display-name).
  ;;   slug         — used for ~/.config/chromium-<slug>, --class=Chromium-<slug>,
  ;;                  and the .desktop filename.
  ;;   display-name — what shows in the application menu and window title.
  '(("ous"      "Chromium (OUS)")
    ("uio"     "Chromium (UiO)")
    ("ntnu"     "Chromium (NTNU)")
    ("personal" "Chromium (Personal)")))

(define (chromium-home-packages)
  "Native ungoogled-chromium for the home profile."
  (list ungoogled-chromium))

(define (profile-desktop-file slug display-name)
  "Build a .desktop launcher for one chromium profile."
  (mixed-text-file
   (string-append "chromium-" slug ".desktop")
   "[Desktop Entry]\n"
   "Version=1.0\n"
   "Type=Application\n"
   "Name=" display-name "\n"
   "GenericName=Web Browser\n"
   ;; sh -c with explicit "$@" forwards URL args from xdg-open / browser=
   ;; integrations.  --user-data-dir keeps each profile fully isolated;
   ;; --class sets WM_CLASS so bspwm rules can target individual profiles
   ;; (e.g. send Chromium-ous to a specific desktop).
   "Exec=sh -c 'exec "
   (file-append ungoogled-chromium "/bin/chromium")
   " --user-data-dir=\"$HOME/.config/chromium-" slug "\""
   " --class=Chromium-" slug
   " \"$@\"' chromium-" slug " %U\n"
   "Icon=chromium\n"
   "Terminal=false\n"
   "StartupNotify=true\n"
   "StartupWMClass=Chromium-" slug "\n"
   "Categories=Network;WebBrowser;\n"
   "MimeType=text/html;text/xml;application/xhtml+xml;application/xml;"
   "x-scheme-handler/http;x-scheme-handler/https;\n"))

(define* (chromium-home-services #:key (profiles %chromium-rafael-profiles))
  "Return the home-service list that installs ungoogled-chromium and one
.desktop launcher per profile.  PROFILES is an alist of (slug display-name)
pairs (default: %chromium-rafael-profiles — OUS / UiO / NTNU / Personal)."
  (list
   (simple-service 'chromium-package
                   home-profile-service-type
                   (chromium-home-packages))

   (simple-service 'chromium-profile-launchers
                   home-files-service-type
                   (map (lambda (entry)
                          (let ((slug         (car entry))
                                (display-name (cadr entry)))
                            (list (string-append ".local/share/applications/"
                                                 "chromium-" slug ".desktop")
                                  (profile-desktop-file slug display-name))))
                        profiles))))
