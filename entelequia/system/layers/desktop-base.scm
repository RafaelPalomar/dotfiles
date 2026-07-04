(define-module (entelequia system layers desktop-base)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib common-services)
  #:use-module (entelequia system lib librewolf-policy)
  #:use-module (entelequia system lib xorg-configs)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services containers)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services security-token)
  #:use-module (gnu system accounts)
  #:use-module (nongnu packages linux)
  #:use-module (guix gexp)
  #:export (make-desktop-base-os))

(use-package-modules wm linux)
(use-service-modules desktop xorg networking ssh security containers linux
                     virtualization)

;;; Desktop base operating system layer
;;;
;;; This layer provides common desktop functionality shared between all
;;; desktop/laptop systems.  It owns everything the machine files used to
;;; copy-paste five times over: the SLiM display manager (with a per-GPU
;;; xlibre configuration derived from machine-config-gpu-type), rootless
;;; podman for the primary user, the librewolf policy, GPU driver packages,
;;; and GPU kernel arguments.  Machines keep only genuinely unique content:
;;; filesystems/UUIDs, extra kernel args, one-off services, and extra user
;;; accounts/groups.

(define* (make-desktop-base-os config
                                #:key
                                (extra-packages '())
                                (extra-services '())
                                (extra-user-groups '("cgroup"))
                                (extra-user-accounts '())
                                (extra-kernel-arguments '())
                                (xorg-config #f)
                                (podman? #t)
                                (librewolf-policy? #t)
                                (firewall-extra-tcp-ports '())
                                (firewall-extra-udp-ports '())
                                (firewall-trusted-subnets '())
                                (ssh-authorized-keys '()))
  "Create a desktop base operating system from a machine-config record.
   CONFIG should be a <machine-config> record.
   EXTRA-PACKAGES and EXTRA-SERVICES for machine-specific additions.
   EXTRA-USER-GROUPS: supplementary groups beyond the base set for the
   primary user (default adds \"cgroup\" for rootless podman).
   EXTRA-USER-ACCOUNTS: additional <user-account> records.
   EXTRA-KERNEL-ARGUMENTS appended to the GPU-derived kernel arguments.
   XORG-CONFIG overrides the per-GPU xlibre configuration.
   PODMAN? / LIBREWOLF-POLICY? opt out of rootless podman / the policy.
   FIREWALL-* and SSH-AUTHORIZED-KEYS forwarded to the base layer."
  (let* ((gpu (machine-config-gpu-type config))
         (username (machine-config-username config))
         (xorg (or xorg-config
                   (make-xlibre-config gpu (machine-config-keyboard config))))
         ;; Desktop services flow through #:extra-services of the base layer
         ;; (NOT an operating-system services override) — that is what
         ;; avoids the old double-inheritance service duplication.
         (desktop-services
          (append
           (list
            ;; SLiM display manager with the per-GPU xlibre configuration.
            (service slim-service-type
                     (slim-configuration
                      (auto-login? #f)
                      (default-user username)
                      (xorg-configuration xorg)))

            ;; ── Desktop-only services moved out of the base layer ────────
            ;; (servers used to inherit all of this: a bluetooth daemon
            ;; with no adapter, a Wayland screen locker on an X11 fleet,
            ;; the NTNU VPN profile, and a fleet-wide GnuTLS TLS-1.3
            ;; disable.  See git history of layers/base.scm.)

            ;; Screen locker PAM service.  NOTE: the actual locker in use
            ;; is setuid slock (privileged program in base + xautolock in
            ;; the home env); swaylock here predates that and is likely
            ;; vestigial — kept for now, candidate for removal.
            (service screen-locker-service-type
                     (screen-locker-configuration
                      (name "swaylock")
                      (program (file-append swaylock "/bin/swaylock"))
                      (using-pam? #t)
                      (using-setuid? #f)))

            ;; Bluetooth (desktops/laptops have adapters; servers don't)
            (service bluetooth-service-type
                     (bluetooth-configuration
                      (auto-enable? #t)))

            ;; Cellular modems + mode-switching USB WWAN devices
            (service modem-manager-service-type)
            (service usb-modeswitch-service-type)

            ;; Battery/power reporting + CUPS polkit helper
            (service upower-service-type)
            (service cups-pk-helper-service-type)

            ;; NTNU VPN: declarative NetworkManager profile + the GnuTLS
            ;; version override its openconnect needs.  Desktop-only: the
            ;; TLS-1.3 disable is a real crypto downgrade and servers have
            ;; no business carrying it (nor the personal VPN profile).
            gnutls-tls-config-service
            ntnu-vpn-connection-service

            ;; Virtualization (virt-manager/QEMU on workstations; servers
            ;; run rootless podman instead)
            (service libvirt-service-type
                     (libvirt-configuration
                      (unix-sock-group "libvirt")
                      (tls-port "16555")))

            ;; v4l2loopback virtual camera (OBS)
            ;; See: https://stackoverflow.com/a/66072635
            (service kernel-module-loader-service-type '("v4l2loopback"))
            (simple-service 'v4l2loopback-config etc-service-type
                            (list `("modprobe.d/v4l2loopback.conf"
                                    ,(plain-file "v4l2loopback.conf"
                                                 "options v4l2loopback devices=1 video_nr=2 exclusive_caps=1 card_label=\"OBS Virtual Camera\"")))))
           (if podman?
               (list (service rootless-podman-service-type
                              (rootless-podman-configuration
                               (subuids (list (subid-range (name username))))
                               (subgids (list (subid-range (name username)))))))
               '())
           (if librewolf-policy?
               (list librewolf-policy-service)
               '())
           extra-services))
         (base-os (make-base-operating-system config
                                              #:extra-services desktop-services
                                              #:extra-user-groups extra-user-groups
                                              #:extra-user-accounts extra-user-accounts
                                              #:firewall-extra-tcp-ports firewall-extra-tcp-ports
                                              #:firewall-extra-udp-ports firewall-extra-udp-ports
                                              #:firewall-trusted-subnets firewall-trusted-subnets
                                              #:ssh-authorized-keys ssh-authorized-keys)))
    ;; Inherit all base fields; override only packages and kernel-arguments.
    (operating-system
     (inherit base-os)

     ;; v4l2loopback kernel module for virtual cameras (desktop-only)
     (kernel-loadable-modules (list v4l2loopback-linux-module))

     ;; GPU-appropriate kernel arguments + machine extras
     (kernel-arguments (gpu-kernel-arguments gpu
                                             #:extra-args extra-kernel-arguments))

     (packages (append
                ;; Base desktop packages
                (specifications->packages
                 (append base-hardware-packages
                         base-audio-packages
                         base-bluetooth-packages
                         base-x11-packages
                         base-filesystem-packages
                         base-security-packages
                         base-virtualization-packages
                         base-monitoring-packages
                         ;; GPU driver/firmware packages from the record
                         (gpu-driver-packages gpu)))
                ;; Machine-specific extra packages
                extra-packages
                ;; Keep base packages
                (operating-system-packages base-os))))))
