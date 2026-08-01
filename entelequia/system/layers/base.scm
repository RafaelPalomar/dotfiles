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
  #:export (make-base-operating-system))

(use-package-modules audio video nfs certs shells ssh linux bash emacs glib gnome
                     networking wm fonts libusb cups freedesktop file-systems
                     version-control package-management vim shellutils vpn suckless)

(use-service-modules dns guix admin sysctl pm nix avahi dbus cups desktop linux
                     mcron networking shepherd xorg ssh docker audio
                     virtualization)

;;; TLP suspend/resume re-apply
;;;
;;; The kernel resets cpufreq state across suspend (secondary CPUs are
;;; hot-unplugged, their policies recreated with defaults), so TLP's
;;; settings must be re-applied on resume -- otherwise every lid-close
;;; brings turbo back and drops the governor to the kernel default
;;; until the next boot (on the X220 that meant idling near the thermal
;;; limit).  The tlp package ships an elogind system-sleep hook, but
;;; elogind only scans its own store directory and
;;; /etc/elogind/system-sleep, and /etc/elogind is a whole-directory
;;; store symlink owned by elogind-service-type -- a nested etc entry
;;; collides with it at build time.  So instead listen for login1's
;;; PrepareForSleep signal on the system bus and drive
;;; `tlp suspend'/`tlp resume' from a dedicated shepherd service.
;;; (The pre-sleep call is advisory -- we hold no inhibitor lock -- but
;;; the one that matters is the post-resume re-apply, which has all the
;;; time it needs.)

(define tlp-sleep-watcher
  (computed-file
   "tlp-sleep-watcher"
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (display (string-append
                     "#!" #$bash-minimal "/bin/sh\n"
                     #$dbus "/bin/dbus-monitor --system "
                     "\"type='signal',sender='org.freedesktop.login1',"
                     "interface='org.freedesktop.login1.Manager',"
                     "member='PrepareForSleep'\" | \\\n"
                     "while IFS= read -r line; do\n"
                     "    case \"$line\" in\n"
                     "        *\"boolean true\"*)  " #$tlp "/bin/tlp suspend ;;\n"
                     "        *\"boolean false\"*) " #$tlp "/bin/tlp resume  ;;\n"
                     "    esac\n"
                     "done\n")
                    port)))
       (chmod #$output #o555))))

(define tlp-sleep-resync-service
  (simple-service
   'tlp-sleep-resync shepherd-root-service-type
   (list (shepherd-service
          (documentation "Re-apply TLP settings around suspend/resume.")
          (provision '(tlp-sleep-resync))
          (requirement '(dbus-system))
          (start #~(make-forkexec-constructor (list #$tlp-sleep-watcher)))
          (stop #~(make-kill-destructor))))))

;;; Parameterized base operating system
;;;
;;; This function creates a base operating system configuration
;;; using a machine-config record for parameterization, eliminating
;;; hardcoded values and enabling easy creation of new systems.

(define* (make-base-operating-system config
                                     #:key
                                     (extra-services '())
                                     (extra-user-groups '())
                                     (extra-user-accounts '())
                                     (firewall-extra-tcp-ports '())
                                     (firewall-extra-udp-ports '())
                                     (firewall-trusted-subnets '())
                                     (enable-ip-forwarding? #f)
                                     (ssh-authorized-keys '()))
  "Create a base operating system from a machine-config record.
   CONFIG should be a <machine-config> record with all required fields.
   EXTRA-SERVICES can be provided to add machine-specific services.
   EXTRA-USER-GROUPS: supplementary groups added to the primary user's
   base set (e.g. '(\"cgroup\" \"lp\" \"dialout\")) — machines no longer
   shadow the record-built account just to add groups.
   EXTRA-USER-ACCOUNTS: additional <user-account> records (multi-user hosts).
   FIREWALL-EXTRA-TCP-PORTS and FIREWALL-EXTRA-UDP-PORTS can be provided for machine-specific firewall rules.
   ENABLE-IP-FORWARDING? enables IP forwarding sysctl and container forwarding nftables rules."
  (operating-system
   (host-name (machine-config-hostname config))
   (timezone (machine-config-timezone config))
   (locale (machine-config-locale config))

   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

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

   (users (append
           (list (user-account
                  (name (machine-config-username config))
                  (comment "User Account")
                  (group "users")
                  (home-directory (string-append "/home/" (machine-config-username config)))
                  (supplementary-groups
                   (append '("wheel"  ;; sudo
                             "netdev" ;; network devices
                             "kvm"
                             "tty"
                             "input"
                             "realtime" ;; Enable realtime scheduling
                             "audio"    ;; control audio devices
                             "video")   ;; control video devices
                           extra-user-groups))))
           extra-user-accounts
           %base-user-accounts))

   ;; Add the 'realtime' group ('cgroup' is already in %base-groups)
   (groups (cons (user-group (system? #t) (name "realtime"))
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


               (simple-service 'guix-moe guix-service-type
                               (guix-extension
                                (authorized-keys
                                 (list (plain-file "guix-moe.pub"
                                                   "(public-key (ecc (curve Ed25519) (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))")
                                       (plain-file "systole.pub"
                                                   "(public-key (ecc (curve Ed25519) (q #4EB06D3040B7AC87026B998030225A9E14DE383FFAD6FAAA87F0B9267321E7BC#)))")))
                                (substitute-urls
                                 '("https://cache-cdn.guix.moe"))))



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
               (service wpa-supplicant-service-type) ;; Needed by NetworkManager

               ;; Basic desktop system services (copied from %desktop-services)
               (service avahi-service-type)
               (service udisks-service-type)
               ;; Note: polkit-wheel-service is defined above (line 139)
               (service dbus-root-service-type)
               fontconfig-file-system-service ;; Manage the fontconfig cache

               ;; Enable JACK to enter realtime mode
               (service pam-limits-service-type
                        (list
                         (pam-limits-entry "@realtime" 'both 'rtprio 99)
                         (pam-limits-entry "@realtime" 'both 'nice -19)
                         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

               ;; Enable hardened SSH access
               (hardened-ssh-service 2222
                                     #:authorized-keys ssh-authorized-keys))

              ;; Security hardening services (kernel, firewall, fail2ban, audit)
              (security-hardening-services #:ssh-port 2222
                                           #:enable-fail2ban? #t
                                           #:enable-firewall? #t
                                           #:enable-audit? #t
                                           #:enable-ip-forwarding? enable-ip-forwarding?
                                           #:firewall-extra-tcp-ports firewall-extra-tcp-ports
                                           #:firewall-extra-udp-ports firewall-extra-udp-ports
                                           #:firewall-trusted-subnets firewall-trusted-subnets)

              ;; Continue with other services
              (list
               ;; Sync system clock with time servers
               (service ntp-service-type)

               ;; Add udev rules for MTP (mobile) devices for non-root user access
               (simple-service 'mtp udev-service-type (list libmtp))

               ;; Add udev rules for a few package
               (udev-rules-service 'pipewire-add-udev-rules pipewire)
               (udev-rules-service 'brightnessctl-udev-rules brightnessctl)

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
              ;;
              ;; energy-perf-policy on AC: TLP default is "balance_performance"
              ;; which leaves the CPU idling at ~1.8 GHz against a 5+ GHz max
              ;; on amd_pstate=active, so the ramp-up to draw a new window
              ;; (kitty open, picom xrender shadow) feels sluggish.  Force
              ;; "performance" on AC for desktop snappiness; keep "power" on
              ;; battery to preserve runtime.  Same EPP interface works for
              ;; intel_pstate.
              ;; The 'performance AC profile above suits well-cooled modern
              ;; CPUs (curie's AMD).  Thermally-limited machines opt into
              ;; 'cool via (cpu-ac-profile 'cool): no turbo on AC, powersave
              ;; governor, capped max perf and power EPP -- caps the frequency
              ;; that otherwise pins an aging Intel laptop (e.g. the X220) near
              ;; its thermal limit at idle.
              ;; Intel laptops additionally get thermald (Intel-specific
              ;; thermal management; a no-op elsewhere, so gated).
              (if (and (eq? (machine-config-machine-type config) 'laptop)
                       (eq? (machine-config-gpu-type config) 'intel))
                  (list (service thermald-service-type))
                  '())
              (if (eq? (machine-config-machine-type config) 'laptop)
                  (list (service tlp-service-type
                                 (if (eq? (machine-config-cpu-ac-profile config) 'cool)
                                     (tlp-configuration
                                      (cpu-boost-on-ac? #f)
                                      ;; schedutil, not powersave: on non-HWP CPUs
                                      ;; (e.g. Sandy Bridge) the kernel runs
                                      ;; intel_pstate in passive mode (intel_cpufreq),
                                      ;; where "powersave" statically pins the MINIMUM
                                      ;; frequency (800 MHz on the X220) -- cool but
                                      ;; unusable.  schedutil scales dynamically under
                                      ;; the cap below; TLP translates cpu-max-perf
                                      ;; into both max_perf_pct and scaling_max_freq,
                                      ;; so turbo stays off either way.
                                      (cpu-scaling-governor-on-ac (list "schedutil"))
                                      (cpu-max-perf-on-ac 60)
                                      (wifi-pwr-on-bat? #t)
                                      ;; cpu-energy-perf-policy-*: TLP >= 1.3 renamed
                                      ;; ENERGY_PERF_POLICY_* (what the deprecated
                                      ;; energy-perf-policy-* fields emit) to
                                      ;; CPU_ENERGY_PERF_POLICY_* and ignores the old
                                      ;; key -- EPB silently stayed at its default.
                                      (cpu-energy-perf-policy-on-ac "power")
                                      (cpu-energy-perf-policy-on-bat "power"))
                                     (tlp-configuration
                                      (cpu-boost-on-ac? #t)
                                      (wifi-pwr-on-bat? #t)
                                      (cpu-energy-perf-policy-on-ac "performance")
                                      (cpu-energy-perf-policy-on-bat "power"))))
                        ;; Re-apply TLP on resume (see tlp-sleep-watcher above).
                        tlp-sleep-resync-service)
                  '())))

   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))

