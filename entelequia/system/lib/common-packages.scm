(define-module (entelequia system lib common-packages)
  #:use-module (entelequia lib helpers)
  #:export (base-hardware-packages
            base-audio-packages
            base-bluetooth-packages
            base-x11-packages
            base-filesystem-packages
            base-security-packages
            base-virtualization-packages
            base-monitoring-packages
            nvidia-specific-packages
            amd-specific-packages
            curie-specific-packages
            einstein-specific-packages))

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
    "gnupg"
    "openvpn"
    "openssl"
    "tailscale"))

;;; Virtualization and containerization packages

(define base-virtualization-packages
  '("qemu"
    "virt-manager"
    "podman"
    "distrobox"))

;;; System monitoring and utility packages

(define base-monitoring-packages
  '("htop"
    "git"
    "vim"
    "direnv"
    "btop"
    "radeontop"
    "coreutils"
    "grep"
    "picom"
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

;;; Machine-specific packages

(define einstein-specific-packages
  '("pulseaudio"
    "fuse-overlayfs"
    "uid-wrapper"
    "slirp4netns"
    "nextcloud-client"))

(define curie-specific-packages
  '("feh"
    "python-pywal"
    "imagemagick"
    "gcc-toolchain"
    "glibc"
    "make"
    "cmake"
    "node"
    "light"))
