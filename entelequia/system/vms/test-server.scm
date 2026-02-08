(define-module (entelequia system vms test-server)
  #:use-module (entelequia lib records)
  #:use-module (entelequia system layers server-base)
  #:use-module (gnu))

;;; Test server VM configuration
;;;
;;; Minimal headless server system for testing in QEMU.
;;; No GUI, no desktop environment - just essential server packages.

(define test-server-config
  (machine-config
   (hostname "test-server")
   (username "test")
   (locale "en_US.utf8")
   (timezone "UTC")
   (keyboard (keyboard-layout "us"))
   (gpu-type #f)  ; No GPU
   (machine-type 'server)))

(define test-server-system
  (operating-system
   (inherit (make-server-base-os test-server-config))

   ;; Minimal kernel arguments for VM
   (kernel-arguments '("quiet" "console=ttyS0"))

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

test-server-system
