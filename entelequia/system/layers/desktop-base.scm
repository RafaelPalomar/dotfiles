(define-module (entelequia system layers desktop-base)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib common-services)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services security-token)
  #:use-module (guix gexp)
  #:export (make-desktop-base-os))

(use-service-modules desktop xorg networking ssh security)

;;; Desktop base operating system layer
;;;
;;; This layer provides common desktop functionality shared between
;;; all desktop systems (einstein, curie), independent of GPU type.
;;; GPU-specific configurations are added in machine-specific files.

(define* (make-desktop-base-os config
                                #:key
                                (extra-packages '())
                                (extra-services '())
                                (firewall-extra-tcp-ports '())
                                (firewall-extra-udp-ports '()))
  "Create a desktop base operating system from a machine-config record.
   CONFIG should be a <machine-config> record.
   EXTRA-PACKAGES and EXTRA-SERVICES can be provided for machine-specific additions.
   FIREWALL-EXTRA-TCP-PORTS and FIREWALL-EXTRA-UDP-PORTS for machine-specific firewall rules."
  (let ((base-os (make-base-operating-system config
                                             #:extra-services extra-services
                                             #:firewall-extra-tcp-ports firewall-extra-tcp-ports
                                             #:firewall-extra-udp-ports firewall-extra-udp-ports)))
    ;; Don't use (inherit base-os) -  explicitly copy only packages field
    ;; This avoids the double-inheritance service duplication bug
    (operating-system
     (inherit base-os) ;; Get all base fields

     ;; Override only packages, leave services unchanged
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
                         base-monitoring-packages))
                ;; Machine-specific extra packages
                extra-packages
                ;; Keep base packages
                (operating-system-packages base-os))))))
