(define-module (entelequia system layers server-base)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:export (make-server-base-os))

(use-service-modules monitoring networking security)

;;; Server base operating system layer
;;;
;;; This layer provides common server functionality for headless systems.
;;; It includes NO X11, NO desktop environment, NO audio - just essential
;;; server packages and services.

(define* (make-server-base-os config
                               #:key
                               (extra-packages '())
                               (extra-services '())
                               (ssh-authorized-keys '())
                               (firewall-extra-tcp-ports '())
                               (firewall-extra-udp-ports '())
                               (enable-ip-forwarding? #f))
  "Create a server base operating system from a machine-config record.
   CONFIG should be a <machine-config> record.
   EXTRA-PACKAGES and EXTRA-SERVICES can be provided for machine-specific additions.
   SSH-AUTHORIZED-KEYS, FIREWALL-EXTRA-TCP-PORTS, FIREWALL-EXTRA-UDP-PORTS and
   ENABLE-IP-FORWARDING? are forwarded to make-base-operating-system."
  (let ((base-os (make-base-operating-system
                  config
                  #:ssh-authorized-keys ssh-authorized-keys
                  #:firewall-extra-tcp-ports firewall-extra-tcp-ports
                  #:firewall-extra-udp-ports firewall-extra-udp-ports
                  #:enable-ip-forwarding? enable-ip-forwarding?)))
    (operating-system
     (inherit base-os)

     ;; Add server-specific packages (monitoring, security, filesystem)
     (packages (append
                (specifications->packages
                 '(;; Monitoring and system tools
                   "htop"
                   "btop"
                   "iotop"
                   "iftop"
                   "ncdu"
                   "tmux"
                   "screen"

                   ;; Security tools
                   "fail2ban"
                   "nftables"
                   "iptables"

                   ;; Networking
                   "curl"
                   "wget"
                   "rsync"
                   "openssh"

                   ;; Development/debugging
                   "git"
                   "vim"
                   "strace"
                   "lsof"

                   ;; Filesystem tools
                   "parted"
                   "smartmontools"
                   "e2fsprogs"
                   "xfsprogs"
                   "btrfs-progs"))
                extra-packages
                (operating-system-packages base-os)))

     ;; Add server-specific services
     (services (append
                (list
                 ;; Prometheus node exporter for system metrics
                 ;; Note: fail2ban and nftables are already added by security-hardening
                 ;; in make-base-operating-system — do not add them here again.
                 (service prometheus-node-exporter-service-type
                          (prometheus-node-exporter-configuration
                           (web-listen-address ":9100"))))

                extra-services
                (operating-system-user-services base-os))))))
