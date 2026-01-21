(define-module (entelequia systems curie)
  #:use-module (entelequia systems base)
  #:use-module (entelequia systems desktop)
  #:use-module (entelequia home-services desktop)
  #:use-module (entelequia home-services emacs)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu home)
  #:use-module (gnu home services sound)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (xlibre))

(use-service-modules cups desktop virtualization networking ssh xorg dbus shepherd security)

(define my-xlibre-config
  (xlibre-configuration
   (modules (list xlibre-video-amdgpu xlibre-input-libinput))
   (drivers '("amdgpu"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (extra-config
    (list
     "
Section \"Device\"
    Identifier \"AMD-GPU\"
    Driver \"amdgpu\"
    Option \"TearFree\" \"on\"
    Option \"AccelMethod\" \"glamor\"
    Option \"DRI\" \"3\"
    Option \"VariableRefresh\" \"true\"
    Option \"EnablePageFlip\" \"true\"
    Option \"ShadowPrimary\" \"false\"
    Option \"ColorTiling\" \"true\"
    Option \"ColorTiling2D\" \"true\"
    Option \"EnableDepthMoves\" \"true\"
    Option \"SwapbuffersWait\" \"true\"
    Option \"TripleBuffer\" \"true\"
EndSection

Section \"Monitor\"
    Identifier \"eDP-1\"
    Option \"PreferredMode\" \"1920x1200\"
    DisplaySize 300 190
    HorizSync 30.0-83.0
    VertRefresh 56.0-76.0
EndSection

Section \"Screen\"
    Identifier \"Screen0\"
    Device \"AMD-GPU\"
    Monitor \"eDP-1\"
    DefaultDepth 24
    SubSection \"Display\"
        Depth 24
        Modes \"1920x1200\" \"1920x1080\" \"1600x1200\" \"1368x768\"
    EndSubSection
EndSection
"
     ))))

(define curie-system
  (operating-system
   (inherit base-operating-system)
   (host-name "curie")

   ;; Packages
   (packages (append  (map specification->package '( ;; Hardware/Drivers
                                                    "acpi-call-linux-module"
                                                    "util-linux"
                                                    "v4l2loopback-linux-module"
                                                    "xlibre-server"
                                                    "xlibre-input-libinput"
                                                    "xlibre-video-amdgpu"
                                                    "amd-microcode"
                                                    "amdgpu-firmware"
                                                    "mesa"
                                                    "mesa-headers"
                                                    "llvm-for-mesa"
                                                    "libva"
                                                    "libva-utils"
                                                    "vulkan-tools"
                                                    "vulkan-loader"
                                                    "linux-firmware"
                                                    "openrgb"

                                                    ;;Bluetooth
                                                    "bluez"
                                                    "bluez-alsa"
                                                    "blueman"

                                                    ;;Audio
                                                    "pipewire"
                                                    "pulseaudio"
                                                    "pulsemixer"
                                                    "alsa-lib"
                                                    "alsa-utils"
                                                    "wireplumber"

                                                    ;; X Utilities
                                                    "xterm"
                                                    "xdpyinfo"
                                                    "xset"
                                                    "xwininfo"
                                                    "xprop"
                                                    "xpra"
                                                    "xkill"
                                                    "setxkbmap"
                                                    "xdg-utils"
                                                    "xrandr"

                                                    ;; File management
                                                    "lf"
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
                                                    "dosfstools"
                                                    "xfsprogs"

                                                    ;; Security/Cryptography/VPN
                                                    "nftables"
                                                    "gnupg"
                                                    "openvpn"
                                                    "openssl"

                                                    ;; Virtualization / Containerization
                                                    "qemu"
                                                    "virt-manager"
                                                    "podman"

                                                    ;; Monitoring and utilities
                                                    "htop"
                                                    "btop"
                                                    "radeontop"
                                                    "coreutils"
                                                    "grep"
                                                    "sed"))
                      %base-packages))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (append
     (list

      ;; To configure OpenSSH, pass an 'openssh-configuration'
      ;; record as a second argument to 'service' below.
      (service openssh-service-type)

      ;; Fail2Ban
      (service fail2ban-service-type)

      ;; AIDE file integrity
      (simple-service 'aide
                      shepherd-root-service-type
                      (list
                       (shepherd-service
                        (provision '(aide))
                        (start #~(make-forkexec-constructor
                                  '("/bin/sh" "-c" "/usr/bin/aide --config=/etc/aide.conf --check")))
                        (stop #~(make-kill-destructor))
                        (auto-start? #f))))
      (guix-home-config
       (home-environment
        (services (cons* (service home-emacs-config-service-type)
                         desktop-home-services))))


      ;; mlocate db
      ;; (simple-service 'mlocate
      ;;                 shepherd-root-service-type
      ;;                 (list
      ;;                  (shepherd-service
      ;;                   (provision '(mlocate))
      ;;                   (start #~(make-forkexec-constructor
      ;;                             '("/usr/bin/updatedb")))
      ;;                   (stop #~(make-kill-destructor))
      ;;                   (auto-start? #f))))


      ;; File Permissions Service for /home and /var/lib/aide (HOME-930 Republican: System: (HOME-9304, FILE-7524)
      (simple-service 'file-permissions
                      shepherd-root-service-type
                      (list
                       (shepherd-service
                        (provision '(file-permissions))
                        (start #~(make-forkexec-constructor
                                  '("/bin/sh" "-c"
                                    "chmod 751 /home && chmod 750 /var/lib/aide")))
                        (stop #~(make-kill-destructor))
                        (auto-start? #t))))

      ;; Bluetooth Service
      ;; Enables automatic Bluetooth device connectivity
      (service bluetooth-service-type
               (bluetooth-configuration
                (auto-enable? #t)))

      ;; Add udev rules for a few package
      (udev-rules-service 'pipewire-add-udev-rules pipewire)
      (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
      ;; Device Authorization Udev Rules
      (udev-rules-service 'device-authorization
                          (udev-rule
                           "99-device-authorize.rules"
                           (string-append
                            "SUBSYSTEM==\"usb\", ATTR{authorized}=\"1\"\n"))
                          #:groups '("plugdev"))

      ;; Blueman D-Bus Service
      ;; Provides D-Bus integration for Blueman Bluetooth manager
      (simple-service 'blueman dbus-root-service-type (list blueman))

      ;; Libvirt Virtualization Service
      ;; Configures libvirt for virtual machine management with Unix socket group
      ;; and TLS port for secure connections
      (service
       libvirt-service-type
       (libvirt-configuration
        (unix-sock-group "libvirt")
        (tls-port "16555")))


      ;; Custom SLiM service with Xlibre
      (service slim-service-type
               (slim-configuration
                (auto-login? #f)
                (default-user "berkeley")
                (xorg-configuration my-xlibre-config))))

     (modify-services
      %desktop-services
      (delete gdm-service-type))))

   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (swap-devices (list (swap-space
                        (target (uuid
                                 "7cc1c7c8-7979-4feb-8964-44a3164cee30")))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "7A6E-89C0"
                                       'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid
                                  "7b9930b3-d217-4932-83d3-d5f555d50332"
                                  'ext4))
                         (type "ext4")) %base-file-systems))))

curie-system
