(define-module (entelequia home profiles networking)
  #:use-module (entelequia packages gns3)
  #:use-module (entelequia packages networking)
  #:use-module (gnu packages)
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
;;;
;;; #:gns3?  Include gns3-gui (defaults to #t).  Off for hosts whose
;;; profile would otherwise conflict with gns3-server's transitive
;;; Python deps (e.g. curie pulls hermes-agent, whose discord-py
;;; demands a newer python-aiohttp than gns3-server propagates).

(define* (networking-home-packages #:key (gns3? #t))
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
   (if gns3?
       (list gns3-gui      ; gns3-gui pulls in gns3-server as a dep
             winbox)       ; MikroTik RouterOS GUI manager
       (list winbox))))
