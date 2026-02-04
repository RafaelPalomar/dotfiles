(define-module (entelequia systems einstein)
  #:use-module (entelequia systems base)
  #:use-module (entelequia systems desktop)
  #:use-module (entelequia home-services desktop)
  #:use-module (entelequia home-services emacs)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages synergy)
  #:use-module (gnu home)
  #:use-module (gnu services containers)
  #:use-module (gnu services networking)
  #:use-module (btv tailscale)
  #:use-module (gnu home services sound)
  #:use-module (gnu system accounts)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (nonguix transformations)
  #:use-module (xlibre))

(use-service-modules cups desktop virtualization networking ssh xorg dbus shepherd security)

(define my-nvidia-xorg-config
  (xorg-configuration
   (modules (list nvidia-driver))
   (drivers '("nvidia"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))))

(define einstein-system
  (operating-system
   (inherit base-operating-system)
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   (host-name "einstein")
   (kernel-arguments '("quiet"
                       "splash"
                       "pcspkr.disable=1"
                       "modprobe.blacklist=nouveau"
                       "nvidia-drm.modeset=1"))


   ;;(kernel-loadable-modules (list nvidia-driver))

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "audio" "lp" "video" "cgroup")))
                 %base-user-accounts))
   (packages (append  (map specification->package '( ;; Hardware/Drivers
                                                    "acpi-call-linux-module"
                                                    "util-linux"
                                                    "v4l2loopback-linux-module"
                                                    "xlibre-server"
                                                    "xlibre-input-libinput"
                                                    "xlibre-input-evdev"
                                                    "mesa"
                                                    "mesa-headers"
                                                    "llvm-for-mesa"
                                                    "libva"
                                                    "libva-utils"
                                                    "vulkan-tools"
                                                    "vulkan-loader"
                                                    "linux-firmware"
                                                    "nvidia-driver"
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
                                                    "setxkbmap"
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
                                                    "xwininfo"

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
                                                    "tailscale"

                                                    ;; Virtualization / Containerization
                                                    "qemu"
                                                    "virt-manager"
                                                    "podman"
                                                    "distrobox"
                                                    "fuse-overlayfs"
                                                    "uid-wrapper"
                                                    "slirp4netns"

                                                    ;; Monitoring and utilities
                                                    "htop"
                                                    "git"
                                                    "vim"
                                                    "btop"
                                                    "radeontop"
                                                    "coreutils"
                                                    "grep"
                                                    "picom"
                                                    "nextcloud-client"
                                                    "sed"))
                      %base-packages))

   (services
    (append
     (list

      ;; To configure OpenSSH, pass an 'openssh-configuration'
      ;; record as a second argument to 'service' below.
      (service openssh-service-type)

      ;; Fail2Ban
      (service fail2ban-service-type)

      ;; Tailscale
      (service tailscale-service-type)

      ;; Elogind
      (service elogind-service-type)

      ;; NetworkManager
      (service network-manager-service-type)

      ;; WPA-supplicant (required by NetworkManager)
      (service wpa-supplicant-service-type)

      ;; Rootless podman
      (service rootless-podman-service-type
               (rootless-podman-configuration
                (subuids (list (subid-range (name "rafael"))))
                (subgids (list (subid-range (name "rafael"))))))

      (service iptables-service-type)

      ;;(service nvidia-service-type)

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
      ;; (simple-service 'etc etc-service-type
      ;;                (list `("containers/registries.conf"
      ;;                        ,(plain-file "registries.conf"
      ;;                                     "unqualified-search-registries=[\"docker.io\"]"))))

      ;; Libvirt Virtualization Service
      ;; Configures libvirt for virtual machine management with Unix socket group
      ;; and TLS port for secure connections
      (service libvirt-service-type
               (libvirt-configuration
                (unix-sock-group "libvirt")
                (tls-port "16555")))

      ;; Custom SLiM service with Xlibre
      (service slim-service-type
               (slim-configuration
                (auto-login? #f)
                (default-user "rafael")  ;; Note: was "berkeley"
                ;; (xorg-configuration my-nvidia-xorg-config)
                )))

     %base-services
     ;; (modify-services
     ;;  %desktop-services
     ;;  (delete gdm-service-type)
     ;;  (delete screen-locker-service-type))
     ))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
   (initrd-modules (append '("vmd") %base-initrd-modules))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "be34730a-1ca8-421f-89e7-4a651f90888c"
                                  'btrfs))
                         (type "btrfs"))
                        (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "E174-0557"
                                       'fat32))
                         (type "vfat")) %base-file-systems))))

((nonguix-transformation-nvidia #:configure-xorg? #f) einstein-system)
