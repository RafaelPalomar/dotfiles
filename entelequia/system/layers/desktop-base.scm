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
  #:use-module (guix gexp)
  #:export (make-desktop-base-os))

(use-service-modules desktop xorg networking ssh security containers)

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
                      (xorg-configuration xorg))))
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
