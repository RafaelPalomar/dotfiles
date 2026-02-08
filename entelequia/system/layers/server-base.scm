(define-module (entelequia system layers server-base)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:export (make-server-base-os))

(use-service-modules networking security)

;;; Server base operating system layer
;;;
;;; This layer provides common server functionality for headless systems.
;;; It includes NO X11, NO desktop environment, NO audio - just essential
;;; server packages and services.

(define* (make-server-base-os config
                               #:key
                               (extra-packages '())
                               (extra-services '()))
  "Create a server base operating system from a machine-config record.
   CONFIG should be a <machine-config> record.
   EXTRA-PACKAGES and EXTRA-SERVICES can be provided for machine-specific additions."
  (let ((base-os (make-base-operating-system config)))
    (operating-system
     (inherit base-os)

     ;; Add server-specific packages (monitoring, security, virtualization)
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
                   "xfsprogs"))
                extra-packages
                (operating-system-packages base-os)))

     ;; Add server-specific services
     (services (append
                (list
                 ;; Fail2Ban for SSH protection
                 (service fail2ban-service-type)

                 ;; IPTables/NFTables
                 (service iptables-service-type))

                extra-services
                (operating-system-services base-os))))))
