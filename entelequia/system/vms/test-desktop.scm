(define-module (entelequia system vms test-desktop)
  #:use-module (entelequia lib records)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (gnu))

;;; Test desktop VM configuration
;;;
;;; Minimal desktop system for testing in QEMU.
;;; Uses generic Intel GPU and minimal package set for fast boot.

(define test-desktop-config
  (machine-config
   (hostname "test-desktop")
   (username "test")
   (locale "en_US.utf8")
   (timezone "UTC")
   (keyboard (keyboard-layout "us"))
   (gpu-type 'intel)
   (machine-type 'desktop)))

(define test-desktop-system
  (operating-system
   (inherit (make-desktop-base-os test-desktop-config))

   ;; Minimal kernel arguments for VM
   (kernel-arguments '("quiet"))

   ;; Use default bootloader configuration
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))))

   ;; Simple file system for VM
   (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                       %base-file-systems))))

test-desktop-system
