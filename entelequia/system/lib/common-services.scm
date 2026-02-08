(define-module (entelequia system lib common-services)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services networking)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages audio)
  #:use-module (guix gexp)
  #:export (aide-service
            file-permissions-service
            desktop-udev-rules-services
            blueman-dbus-service))

;;; Common service definitions shared between desktop systems
;;;
;;; This module extracts the 7+ duplicate service definitions
;;; from einstein and curie, providing a single source of truth.

;;; AIDE file integrity monitoring service

(define aide-service
  (simple-service 'aide
                  shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(aide))
                    (start #~(make-forkexec-constructor
                              '("/bin/sh" "-c" "/usr/bin/aide --config=/etc/aide.conf --check")))
                    (stop #~(make-kill-destructor))
                    (auto-start? #f)))))

;;; File permissions service for /home and /var/lib/aide

(define file-permissions-service
  (simple-service 'file-permissions
                  shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(file-permissions))
                    (start #~(make-forkexec-constructor
                              '("/bin/sh" "-c"
                                "chmod 751 /home && chmod 750 /var/lib/aide")))
                    (stop #~(make-kill-destructor))
                    (auto-start? #t)))))

;;; Udev rules for desktop devices
;;; Note: pipewire and brightnessctl udev rules are already in base.scm

(define desktop-udev-rules-services
  (list
   ;; Device authorization udev rules
   (udev-rules-service 'device-authorization
                       (udev-rule
                        "99-device-authorize.rules"
                        (string-append
                         "SUBSYSTEM==\"usb\", ATTR{authorized}=\"1\"\n"))
                       #:groups '("plugdev"))))

;;; Blueman D-Bus integration service

(define blueman-dbus-service
  (simple-service 'blueman dbus-root-service-type (list blueman)))

;;; Note: polkit-wheel-service is provided by (gnu services desktop)
;;; and is used directly in base.scm - no custom definition needed
;;;
;;; Note: bluetooth-service-type and libvirt-service-type are already
;;; configured in base.scm - no need for separate definitions here
