(define-module (entelequia home-services desktop)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu home services)
  #:use-module (entelequia packages fonts)
  #:use-module (nongnu packages fonts)
  #:use-module (gnu packages fonts)
  #:export (home-desktop-service-type))

(use-package-modules admin chromium compression curl dns fonts freedesktop gimp glib gnome
                     gnome-xyz gnupg gstreamer package-management kde-frameworks librewolf
		     linux lsof music password-utils pdf pulseaudio ssh syncthing terminals
		     tmux video wget wm xdisorg)

(define (home-desktop-profile-service config)
  (list bspwm
        sxhkd
        alacritty
        gammastep
        network-manager-applet
        dunst
        polybar
        rofi
	pinentry-rofi

        ;; Flatpak and XDG utilities
        flatpak
        ;;TODO Review this package pinning
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

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "My desktop environment service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))
