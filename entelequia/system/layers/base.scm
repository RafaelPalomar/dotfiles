(define-module (entelequia system layers base)
  #:use-module (entelequia lib records)
  #:use-module (entelequia system lib common-services)
  #:use-module (entelequia system lib security-hardening)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu system privilege)
  #:use-module (nongnu packages linux)
  #:use-module (btv tailscale)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix gexp)
  #:export (make-base-operating-system
            guix-home-config))

(use-package-modules audio video nfs certs shells ssh linux bash emacs gnome
                     networking wm fonts libusb cups freedesktop file-systems
                     version-control package-management vim shellutils vpn suckless)

(use-service-modules dns guix admin sysctl pm nix avahi dbus cups desktop linux
                     mcron networking xorg ssh docker audio virtualization)

;;; Parameterized base operating system
;;;
;;; This function creates a base operating system configuration
;;; using a machine-config record for parameterization, eliminating
;;; hardcoded values and enabling easy creation of new systems.

(define* (make-base-operating-system config
                                     #:key
                                     (extra-services '())
                                     (firewall-extra-tcp-ports '())
                                     (firewall-extra-udp-ports '()))
  "Create a base operating system from a machine-config record.
   CONFIG should be a <machine-config> record with all required fields.
   EXTRA-SERVICES can be provided to add machine-specific services.
   FIREWALL-EXTRA-TCP-PORTS and FIREWALL-EXTRA-UDP-PORTS can be provided for machine-specific firewall rules."
  (operating-system
   (host-name (machine-config-hostname config))
   (timezone (machine-config-timezone config))
   (locale (machine-config-locale config))

   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   ;; Additional kernel modules
   (kernel-loadable-modules (list v4l2loopback-linux-module))

   ;; Use keyboard layout from config
   (keyboard-layout (machine-config-keyboard config))

   ;; Use the UEFI variant of GRUB with the EFI System
   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout (machine-config-keyboard config))))

   ;; Guix doesn't like it when there isn't a file-systems
   ;; entry, so add placeholders that are meant to be overridden
   (file-systems (cons*
                  ;; Placeholder root filesystem (override in machine config)
                  (file-system
                   (mount-point "/")
                   (device "none")
                   (type "tmpfs"))
                  ;; Hardened /tmp mount with security flags
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f)
                   (flags '(no-dev no-suid no-exec))  ; Security: prevent execution and setuid
                   (options "mode=1777,strictatime"))  ; World-writable with sticky bit
                  %base-file-systems))

   (users (cons (user-account
                 (name (machine-config-username config))
                 (comment "User Account")
                 (group "users")
                 (home-directory (string-append "/home/" (machine-config-username config)))
                 (supplementary-groups '("wheel"  ;; sudo
                                         "netdev" ;; network devices
                                         "kvm"
                                         "tty"
                                         "input"
                                         "realtime" ;; Enable realtime scheduling
                                         "audio"    ;; control audio devices
                                         "video"))) ;; control video devices

                %base-user-accounts))

   ;; Add the 'realtime' and 'cgroup' groups
   (groups (cons* (user-group (system? #t) (name "realtime"))
                  (user-group (system? #t) (name "cgroup"))
                  %base-groups))

   ;; Install bare-minimum system packages
   (packages (cons* exfat-utils
                    fuse-exfat
                    git
                    gvfs    ;; Enable user mounts
                    libva-utils
                    ntfs-3g
                    vim
                    direnv
                    tailscale
                    %base-packages))

   ;; Configure only the services necessary to run the system
   (services (append
              ;; Add any extra services passed to this function
              extra-services
              (modify-services %base-services
                               ;; Remove console-font-service-type as we configure it manually below
                               (delete console-font-service-type))
              (list
               ;; Seat management (can't use seatd because Wireplumber depends on elogind)
               (service elogind-service-type)

               ;; Tailscale
               (service tailscale-service-type)

               ;; Configure TTYs and graphical greeter
               (service console-font-service-type
                        (map (lambda (tty)
                               ;; Use a larger font for HIDPI screens
                               (cons tty (file-append
                                          font-terminus
                                          "/share/consolefonts/ter-132n")))
                             '("tty1" "tty2" "tty3")))


               ;; Configure swaylock as a setuid program
               (service screen-locker-service-type
                        (screen-locker-configuration
                         (name "swaylock")
                         (program (file-append swaylock "/bin/swaylock"))
                         (using-pam? #t)
                         (using-setuid? #f)))

               ;; Configure the Guix service and ensure we use Nonguix substitutes
               (simple-service 'add-nonguix-substitutes
                               guix-service-type
                               (guix-extension
                                (substitute-urls
                                 (cons* "https://substitutes.nonguix.org"
                                        %default-substitute-urls))
                                (authorized-keys
                                 (append (list (plain-file "nonguix.pub"
                                                           "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                         %default-authorized-guix-keys))))

               ;; Set up Polkit to allow `wheel' users to run admin tasks
               polkit-wheel-service
               networkmanager-polkit-service

               ;; Give certain programs super-user access
               (simple-service 'mount-setuid-helpers
                               privileged-program-service-type
                               (map (lambda (program)
                                      (privileged-program
                                       (program program)
                                       (setuid? #t)))
                                    (list (file-append nfs-utils "/sbin/mount.nfs")
                                          (file-append ntfs-3g "/sbin/mount.ntfs-3g")
                                          (file-append slock "/bin/slock"))))

               ;; Networking services
               (service network-manager-service-type
                        (network-manager-configuration
                         (vpn-plugins
                          (list network-manager-openvpn
                                network-manager-openconnect))))
               gnutls-tls-config-service  ;; Configure GnuTLS for NTNU VPN compatibility
               (service wpa-supplicant-service-type) ;; Needed by NetworkManager
               (service modem-manager-service-type)  ;; For cellular modems
               (service bluetooth-service-type
                        (bluetooth-configuration
                         (auto-enable? #t)))
               (service usb-modeswitch-service-type)

               ;; Basic desktop system services (copied from %desktop-services)
               (service avahi-service-type)
               (service udisks-service-type)
               (service upower-service-type)
               (service cups-pk-helper-service-type)
               ;; Note: polkit-wheel-service is defined above (line 139)
               (service dbus-root-service-type)
               fontconfig-file-system-service ;; Manage the fontconfig cache

               ;; Enable JACK to enter realtime mode
               (service pam-limits-service-type
                        (list
                         (pam-limits-entry "@realtime" 'both 'rtprio 99)
                         (pam-limits-entry "@realtime" 'both 'nice -19)
                         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

               ;; Configure v4l2loopback module for virtual cameras
               ;; See also: https://stackoverflow.com/a/66072635
               ;;           https://github.com/umlaeute/v4l2loopback
               (service kernel-module-loader-service-type '("v4l2loopback"))
               (simple-service 'v4l2loopback-config etc-service-type
                               (list `("modprobe.d/v4l2loopback.conf"
                                       ,(plain-file "v4l2loopback.conf"
                                                    "options v4l2loopback devices=1 video_nr=2 exclusive_caps=1 card_label=\"OBS Virtual Camera\""))))


               ;; Enable virtualization
               (service libvirt-service-type
                        (libvirt-configuration
                         (unix-sock-group "libvirt")
                         (tls-port "16555")))

               ;; Enable hardened SSH access
               (hardened-ssh-service 2222))

              ;; Security hardening services (kernel, firewall, fail2ban, audit)
              (security-hardening-services #:ssh-port 2222
                                           #:enable-fail2ban? #t
                                           #:enable-firewall? #t
                                           #:enable-audit? #t
                                           #:firewall-extra-tcp-ports firewall-extra-tcp-ports
                                           #:firewall-extra-udp-ports firewall-extra-udp-ports)

              ;; Continue with other services
              (list
               ;; Sync system clock with time servers
               (service ntp-service-type)

               ;; Add udev rules for MTP (mobile) devices for non-root user access
               (simple-service 'mtp udev-service-type (list libmtp))

               ;; Add udev rules for a few package
               (udev-rules-service 'pipewire-add-udev-rules pipewire)
               (udev-rules-service 'brightnessctl-udev-rules brightnessctl)

               (simple-service 'profile-env-vars-service
                               home-environment-variables-service-type
                               '( ;; Sort hidden (dot) files first in `ls` listings
                                 ("LC_COLLATE" . "C")

                                 ;; Emacs is our editor
                                 ("VISUAL" . "emacsclient")
                                 ("EDITOR" . "emacsclient")

                                 ;; Add some things to $PATH (maybe integrate into other services?)
                                 ("PATH" . "$HOME/.bin:$HOME/.npm-global/bin:$PATH")

                                 ;; Make sure Flatpak apps are visible
                                 ;; TODO: Maybe move to a desktop layer
                                 ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))

               ;; Schedule cron jobs for system tasks
               (simple-service 'system-cron-jobs
                               mcron-service-type
                               (list
                                ;; Run `guix gc' 5 minutes after midnight every day.
                                ;; Clean up generations older than 2 months and free
                                ;; at least 10G of space.
                                #~(job "5 0 * * *" "guix gc -d 2m -F 10G")

                                ;; Run fstrim weekly (Sundays at 3 AM) for SSD health
                                #~(job "0 3 * * 0" "fstrim -av"))))

              ;; Power management services (laptop-only)
              ;; Note: thermald removed - Intel-specific, not needed on AMD
              ;; AMD Zen 5 uses kernel Powercap thermal management
              (if (eq? (machine-config-machine-type config) 'laptop)
                  (list (service tlp-service-type
                                 (tlp-configuration
                                  (cpu-boost-on-ac? #t)
                                  (wifi-pwr-on-bat? #t))))
                  '())))

   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))

(define* (guix-home-config config home-environment)
  "Helper function to create a guix-home service for the configured user.
   CONFIG should be the machine-config record.
   HOME-ENVIRONMENT should be the home-environment configuration."
  (service guix-home-service-type
           `((,(machine-config-username config) ,home-environment))))
