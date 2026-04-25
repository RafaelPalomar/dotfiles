(define-module (entelequia system lib common-packages)
  #:use-module (entelequia lib helpers)
  #:export (base-hardware-packages
            edison-specific-packages
            base-audio-packages
            base-bluetooth-packages
            base-x11-packages
            base-filesystem-packages
            base-security-packages
            base-virtualization-packages
            base-monitoring-packages
            nvidia-specific-packages
            amd-specific-packages
            base-latex-packages
            curie-specific-packages
            einstein-specific-packages
            alucard-specific-packages
            lovelace-specific-packages))

;;; Common package lists shared between desktop systems
;;;
;;; This module extracts the 90% of packages that are identical
;;; between einstein and curie, eliminating duplication.

;;; Hardware packages (shared between desktop systems)

(define base-hardware-packages
  '("acpi-call-linux-module"
    "util-linux"
    "v4l2loopback-linux-module"
    "xlibre-server"
    "xlibre-input-libinput"
    "mesa"
    "mesa-headers"
    "llvm-for-mesa"
    "libva"
    "libva-utils"
    "vulkan-tools"
    "vulkan-loader"
    "linux-firmware"
    "openrgb"))

;;; Bluetooth packages

(define base-bluetooth-packages
  '("bluez"
    "bluez-alsa"
    "blueman"))

;;; Audio packages

(define base-audio-packages
  '("pipewire"
    "pulsemixer"
    "alsa-lib"
    "alsa-utils"
    "wireplumber"))

;;; X11 utilities

(define base-x11-packages
  '("setxkbmap"
    "synergy"
    "xdg-utils"
    "xdpyinfo"
    "xkill"
    "xpra"
    "xprop"
    "xrandr"
    "xset"
    "xsetroot"
    "xterm"
    "xwininfo"))

;;; File management packages

(define base-filesystem-packages
  '("lf"
    "ncdu"
    "mergerfs"
    "parted"
    "ntfs-3g"
    "exfat-utils"
    "exfatprogs"
    "fuse-exfat"
    "dosfstools"
    "bcachefs-tools"
    "smartmontools"
    "e2fsprogs"
    "xfsprogs"))

;;; Security, cryptography, and VPN packages

(define base-security-packages
  '("nftables"
    "fail2ban"
    "audit"
    "gnupg"
    "openvpn"
    "openssl"
    "tailscale"
    "network-manager"              ; Provides NetworkManager daemon and icons
    "network-manager-openconnect"  ; OpenConnect VPN support for NTNU
    "openconnect"))                ; OpenConnect VPN client

;;; Virtualization and containerization packages

(define base-virtualization-packages
  '("qemu"
    "virt-manager"))

;;; System monitoring and utility packages

(define base-monitoring-packages
  ;; btop and picom moved to home/services/desktop.scm (desktop hosts)
  ;; and home/profiles/server.scm (server hosts) so .desktop files aren't
  ;; duplicated in the user's app menu.
  '("htop"
    "git"
    "vim"
    "direnv"
    "radeontop"
    "coreutils"
    "grep"
    "sed"
    "powertop"
    "acpi"))

;;; GPU-specific packages

(define nvidia-specific-packages
  '("nvidia-driver"
    "xlibre-input-evdev"))

(define amd-specific-packages
  '("xlibre-video-amdgpu"
    "amd-microcode"
    "amdgpu-firmware"))

;;; LaTeX / XeLaTeX toolchain (system-level for Emacs AUCTeX and other daemons)
;;;
;;; These are placed at the system level so they are on PATH for all processes,
;;; including the Emacs shepherd service which does not inherit the home-profile PATH.

(define base-latex-packages
  '("texlive-xetex"         ;; XeLaTeX engine
    "texlive-fontspec"      ;; font loading for XeLaTeX/LuaLaTeX
    "texlive-xcolor"        ;; colour support
    "texlive-titlesec"      ;; section heading formatting
    "texlive-hyperref"      ;; hyperlinks and PDF metadata
    "texlive-caption"       ;; subcaption + caption formatting
    "texlive-biblatex"      ;; bibliography management
    "texlive-biber"         ;; biber bibliography backend
    "texlive-setspace"      ;; line spacing control
    "texlive-fancyhdr"      ;; custom headers and footers
    "texlive-wrapfig"       ;; text wrapping around figures
    "texlive-booktabs"      ;; professional-quality tables
    "texlive-tools"         ;; tabularx and other table tools
    "texlive-float"         ;; improved float placement [H]
    "texlive-pgfgantt"      ;; Gantt charts (pgf/tikz)
    "texlive-scheme-basic"  ;; pdflatex + base LaTeX packages
    "texlive-pgfgantt"      ;; Gantt charts (used in proposals, org-babel)
    "texlive-standalone"    ;; standalone class for org-babel latex blocks
    ;; Fonts used by nfr.cls
    "font-dosis"
    "font-sil-gentium"
    "font-sil-gentium-book"))

;;; Machine-specific packages

(define einstein-specific-packages
  '("pulseaudio"
    "fuse-overlayfs"
    "uid-wrapper"
    "slirp4netns"
    "feh"
    "python-pywal"
    "imagemagick"
    "gcc-toolchain"
    "glibc"
    "make"
    "cmake"
    "node"
    "light"
    "jq"
    "nextcloud-client"))

(define curie-specific-packages
  '("feh"
    "python-pywal"
    "imagemagick"
    "gcc-toolchain"
    "nextcloud-client"
    "glibc"
    "make"
    "cmake"
    "node"
    "light"
    "jq"
    "book-sicp"
    "sakura"))

;;; Alucard-specific packages (shared NVIDIA desktop, multi-user)

(define alucard-specific-packages
  '("feh"
    "python-pywal"
    "imagemagick"
    "nextcloud-client"
    "light"
    "jq"
    "fuse-overlayfs"
    "uid-wrapper"
    "slirp4netns"))

;;; Edison-specific packages (multimedia server, NVIDIA, optical ripping)

(define edison-specific-packages
  '(;; Container runtime support
    "fuse-overlayfs"
    "passt"        ; pasta/passt — rootless Podman network backend
    "slirp4netns"
    "crun"
    ;; Filesystem
    "xfsprogs"     ; XFS tools for /data disk (sdb1)
    "nfs-utils"    ; NFS client utilities (mount.nfs is setuid via base)
    ;; Monitoring / diagnostics
    "lm-sensors"
    "ethtool"
    "tcpdump"
    "jq"
    ;; Backup / remote access
    "borgmatic"
    "openssh"))

;;; Lovelace-specific packages (headless server)

(define lovelace-specific-packages
  '(;; Container runtime support
    "fuse-overlayfs"
    "passt"        ; pasta/passt — rootless Podman network backend (required for gluetun)
    "slirp4netns"
    "crun"
    ;; Filesystem
    "btrfs-progs"
    ;; Monitoring / diagnostics
    "lm-sensors"
    "ethtool"
    "tcpdump"
    ;; Backup
    "borgmatic"
    "openssh"    ; ssh client for borg to Hetzner
    "jq"         ; parse borgmatic info --json for Prometheus metrics
    ;; Luanti game server + Mineclonia game (minetest-game is deprecated)
    "luanti-server"
    "luanti-mineclonia"))
