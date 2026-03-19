(define-module (entelequia system vms test-lovelace)
  #:use-module (entelequia lib records)
  #:use-module (entelequia system layers server-base)
  #:use-module (entelequia system lib server-services)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:export (test-lovelace-system))

;;; VM test configuration for lovelace
;;;
;;; Boots the server base with native services only (no OCI containers,
;;; no sops, no btrfs). Used to verify the base system before deploying
;;; to real hardware.
;;;
;;; Launch with:
;;;   ./scripts/test-vm.sh test-lovelace

(define test-lovelace-config
  (machine-config
   (hostname "lovelace-test")
   (username "rafael")
   (gpu-type #f)
   (machine-type 'server)))

(define test-lovelace-system
  (operating-system
   (inherit (make-server-base-os
             test-lovelace-config
             #:extra-services
             (append
              smartd-lovelace-service
              luanti-lovelace-service)))

   ;; VM bootloader (not EFI)
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/vda"))))

   ;; Minimal file systems for VM
   (file-systems (cons (file-system
                         (mount-point "/")
                         (device (file-system-label "my-root"))
                         (type "ext4"))
                       %base-file-systems))))

test-lovelace-system
