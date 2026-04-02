(define-module (entelequia home profiles networking)
  #:use-module (entelequia packages gns3)
  #:use-module (entelequia packages networking)
  #:use-module (gnu packages)
  #:use-module (gnu packages wine)
  #:export (networking-home-packages))

;;; Networking home profile
;;;
;;; Network emulation, analysis, and management tools.
;;; GNS3 requires QEMU (system-level) for VM-based devices.
;;; MikroTik RouterOS and other appliances can be imported via
;;; GNS3's appliance manager once it is running.
;;;
;;; MikroTik tools:
;;;   winbox  — WinBox 4, native Linux Qt GUI for RouterOS management
;;;   wine64  — Wine (64-bit) for running legacy winbox3 .exe or other
;;;             Windows network tools

(define (networking-home-packages)
  (append
   (map specification->package
        '(;; Traffic analysis
          "wireshark"
          ;; Terminal-based packet inspector
          "tcpdump"
          ;; Network scanning
          "nmap"
          ;; SSH tunnelling and port forwarding
          "autossh"))
   (list gns3-gui         ; gns3-gui pulls in gns3-server as a dep
         winbox            ; MikroTik RouterOS GUI manager (native Linux, WinBox 4)
         wine64)))
