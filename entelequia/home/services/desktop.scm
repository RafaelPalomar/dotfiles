(define-module (entelequia home services desktop)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services shepherd)
  #:use-module (entelequia packages fonts)
  #:use-module (nongnu packages fonts)
  #:use-module (gnu packages fonts)
  #:export (home-desktop-service-type))

(use-package-modules admin chromium compton compression curl disk dns fonts freedesktop gimp glib gnome
                     gnome-xyz gnupg gstreamer package-management kde-frameworks librewolf
                     linux lsof music password-utils pdf pulseaudio ssh syncthing terminals
                     tmux video wget wm xdisorg suckless)

;;; Desktop home service
;;;
;;; Provides desktop environment packages and settings.
;;; Refactored from home-services/desktop.scm with bash/GPG config
;;; moved to separate services (shell.scm, gpg.scm).

(define (home-desktop-profile-service config)
  (list bspwm
        sxhkd
        kitty
        picom
        gammastep
        network-manager-applet
        dunst
        libnotify  ;; Provides notify-send for desktop notifications
        polybar
        rofi
        pinentry-rofi
        slock
        xautolock

        ;; Flatpak and XDG utilities
        flatpak
        xdg-desktop-portal
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
        xdg-utils ;; For xdg-open, etc
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        breeze-icons ;; For KDE apps
        gnome-themes-extra
        adwaita-icon-theme

        ;; Fonts
        font-iosevka-ss08
        font-iosevka-aile
        font-microsoft-cascadia
        font-jetbrains-mono
        font-google-material-design-icons
        font-google-noto
        font-google-noto-emoji
        font-liberation
        font-awesome
        font-awesome-nonfree
        font-atui-feather
        polybar-themes-fonts
        nerd-font-iosevka
        nerd-font-jetbrains

        ;; Remote terminal access
        tmux

        ;; Browsers
        librewolf
        ungoogled-chromium

        ;; Authentication
        password-store

        ;; Audio devices and media playback
        mpv
        mpv-mpris
        yt-dlp
        playerctl
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        alsa-utils
        pavucontrol

        ;; Graphics
        gimp

        ;; PDF reader
        zathura
        zathura-pdf-mupdf

        ;; File syncing
        syncthing-gtk

        ;; USB disk management
        udiskie        ;; Automatic USB disk mounting with notifications

        ;; General utilities
        curl
        wget
        openssh
        zip
        unzip

        ;; System monitoring and network tools
        btop
        lsof
        netcat
        (specification->package+output "bind" "utils")))

(define (home-desktop-environment-variables config)
  '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))

(define (home-desktop-shepherd-service config)
  "Return shepherd services for desktop utilities."
  (list
   (shepherd-service
    (documentation "Picom compositor with GLX backend for AMD GPU")
    (provision '(picom))
    (start #~(make-forkexec-constructor
              (list #$(file-append picom "/bin/picom")
                    "--config"
                    (string-append (getenv "HOME")
                                   "/.config/picom/picom-"
                                   (let ((hostname (gethostname)))
                                     (cond
                                      ((string=? hostname "einstein") "einstein")
                                      ((string=? hostname "curie") "curie")
                                      (else "curie")))
                                   ".conf"))
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/picom.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))

   (shepherd-service
    (documentation "Gammastep color temperature adjustment for Oslo")
    (provision '(gammastep))
    (start #~(make-forkexec-constructor
              (list #$(file-append gammastep "/bin/gammastep")
                    "-l" "59.9:10.8"    ; Oslo coordinates
                    "-t" "6500:3500"    ; Day:Night color temperature
                    "-b" "1.0:0.8")     ; Day:Night brightness
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/gammastep.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))

   (shepherd-service
    (documentation "Auto-lock screen after inactivity")
    (provision '(xautolock))
    (start #~(make-forkexec-constructor
              (list #$(file-append xautolock "/bin/xautolock")
                    "-time" "10"        ; Lock after 10 minutes
                    "-locker" #$(file-append slock "/bin/slock")
                    "-detectsleep")     ; Don't lock on suspend
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/xautolock.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))

   (shepherd-service
    (documentation "Automatic USB disk mounting with notifications")
    (provision '(udiskie))
    (start #~(make-forkexec-constructor
              (list #$(file-append udiskie "/bin/udiskie")
                    "--tray"           ; System tray applet
                    "--notify"         ; Desktop notifications
                    "--automount")     ; Auto-mount on insertion
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/udiskie.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))

   (shepherd-service
    (documentation "NetworkManager system tray applet")
    (provision '(nm-applet))
    (start #~(make-forkexec-constructor
              (list #$(file-append network-manager-applet "/bin/nm-applet")
                    "--indicator")     ; Use indicator mode for system tray
              #:log-file (string-append
                         (or (getenv "XDG_STATE_HOME")
                             (string-append (getenv "HOME") "/.local/state"))
                         "/nm-applet.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "Desktop environment packages and settings")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)
                       (service-extension
                        home-shepherd-service-type
                        home-desktop-shepherd-service)))
                (default-value #f)))
