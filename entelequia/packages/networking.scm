(define-module (entelequia packages networking)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)   ; unzip, zlib
  #:use-module (gnu packages fontutils)     ; freetype, fontconfig
  #:use-module (gnu packages gl)            ; mesa
  #:use-module (gnu packages glib)          ; dbus
  #:use-module (gnu packages xdisorg)       ; libxkbcommon
  #:use-module (gnu packages xorg)          ; libx11, libxcb, xcb-util-*
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:export (winbox))

;;; WinBox 4 — native Linux binary from MikroTik.
;;;
;;; Deps (from readelf -d WinBox):
;;;   xcb family (libxcb, xcb-util-{image,keysyms,renderutil,wm})
;;;   libxkbcommon + libxkbcommon-x11
;;;   libX11 + libX11-xcb
;;;   libEGL + libGL (mesa)
;;;   freetype, fontconfig
;;;   dbus, zlib, glibc

(define-public winbox
  (package
    (name "winbox")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.mikrotik.com/routeros/winbox/"
                           version "/WinBox_Linux.zip"))
       (sha256
        (base32 "0w3a69zgabcivmhlkd9grmz0a2znpma367w8i15lqhzx564x1hlf"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f            ; pre-built binary; skip RPATH audit
      #:patchelf-plan
      #~'(("WinBox"
           ("dbus" "fontconfig" "freetype" "libx11" "libxcb"
            "mesa" "xcb-util-image" "xcb-util-keysyms"
            "xcb-util-renderutil" "xcb-util-wm" "libxkbcommon"
            "zlib")))
      #:install-plan
      #~'(("WinBox" "bin/winbox")
          ("assets/img/winbox.png"
           "share/icons/hicolor/256x256/apps/winbox.png"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            ;; The upstream archive is a flat zip — extract in place.
            (lambda* (#:key source #:allow-other-keys)
              (invoke #$(file-append unzip "/bin/unzip") "-o" source)
              (chmod "WinBox" #o755)))
          (add-after 'install 'create-desktop-file
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (mkdir-p apps)
                (with-output-to-file (string-append apps "/winbox.desktop")
                  (lambda ()
                    (format #t "[Desktop Entry]~%")
                    (format #t "Version=1.0~%")
                    (format #t "Type=Application~%")
                    (format #t "Name=WinBox~%")
                    (format #t "GenericName=MikroTik Router Manager~%")
                    (format #t "Comment=MikroTik RouterOS GUI management tool~%")
                    (format #t "Exec=~a/bin/winbox~%" #$output)
                    (format #t "Icon=winbox~%")
                    (format #t "Terminal=false~%")
                    (format #t "Categories=Network;System;~%")))))))))
    (native-inputs (list unzip))
    (inputs
     (list dbus
           fontconfig
           freetype
           libx11
           libxcb
           mesa
           xcb-util-image
           xcb-util-keysyms
           xcb-util-renderutil
           xcb-util-wm
           libxkbcommon
           zlib))
    (supported-systems '("x86_64-linux"))
    (synopsis "MikroTik RouterOS GUI management tool")
    (description
     "WinBox is a GUI utility for administering MikroTik RouterOS.  It
provides a fast and simple interface for all RouterOS configuration tasks,
including routing, firewall, VPN, wireless, and more.

This is WinBox 4, a native Linux (and Windows/macOS) application built
with Qt.  It connects directly to MikroTik devices via Winbox protocol
(TCP 8291) or MAC address discovery on the local network.")
    (home-page "https://mikrotik.com/download")
    (license (license:nonfree "https://mikrotik.com/downloadterms.html"))))
