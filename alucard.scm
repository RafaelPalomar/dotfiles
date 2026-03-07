;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
             (gnu services base)
             (systole transformations)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (guix channels))
(use-service-modules cups desktop networking ssh xorg)

;; Channel specifications for reproducible system updates.
;; These channels are embedded from the installer and used by 'guix pull'
;; and 'guix system reconfigure'.
(define %system-channels
  (list (channel
          (name 'guix)
          (url "https://codeberg.org/guix/guix.git")
          (branch "master")
          (commit "ebe4cc6b3b7c02f691c4ce236a9a7c98746205a4")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")
          (branch "master")
          (commit "cbf5bd14cc0888caee8cf6e6abd9ec7c111307d7")
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
          (name 'guix-xlibre)
          (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
          (branch "master")
          (commit "02b15dba1951803649c95bdc17feca2b544fc4ee"))
        (channel
          (name 'tailscale)
          (url "https://github.com/umanwizard/guix-tailscale")
          (branch "main")
          (commit "58bc8b05520b8565a3230e21388e97f00b886e4b"))
        (channel
          (name 'guix-systole)
          (url "https://github.com/systoleos/guix-systole")
          (branch "main")
          (commit "3b428109bf9bbad116443bc5664dae3533d2ac4f"))
        (channel
          (name 'systole-artwork)
          (url "https://github.com/systoleos/guix-systole-artwork")
          (branch "main")
          (commit "26e4f71bf518a03c646d42d7c65ec8529f3c63a6"))))

;; Optional: Add Systole branding to GRUB bootloader.
;; Uncomment the following lines and add (systole packages grub-themes) to use-modules:
;; In bootloader-configuration, add:
;;   (theme (grub-theme
;;           (image (file-append systole-grub-theme
;;                  "/share/grub/themes/systole/systole.png"))))
((compose (systole-transformation-guix #:guix-source? #t)
          (systole-transformation-linux #:initrd base-initrd))
 (operating-system
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   (host-name "alucard")

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                   (name "rafael")
                   (comment "Rafael Palomar")
                   (group "users")
                   (home-directory "/home/rafael")
                   (supplementary-groups '("wheel" "netdev" "audio" "video")))
                 (user-account
                   (name "leandro")
                   (comment "Leandro Palomar")
                   (group "users")
                   (home-directory "/home/leandro")
                   (supplementary-groups '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (modify-services (append (list

                                   ;; To configure OpenSSH, pass an 'openssh-configuration'
                                   ;; record as a second argument to 'service' below.
                                   (service openssh-service-type
                                            (openssh-configuration (permit-root-login 'prohibit-password)
                                                                   (authorized-keys `
                                                                    (("root" ,
                                                                      (plain-file
                                                                       "deploy-key.pub"
                                                                       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1k6qoXg+tPB5tQjDu690RvaICgd8TJYWPCp+U9UJTi rafael@curie"))))))
                                   (service network-manager-service-type)
                                   (service wpa-supplicant-service-type)
                                   (service ntp-service-type))

                             ;; This is the default list of services we
                             ;; are appending to.
                             %base-services)
      (guix-service-type config =>
                         (guix-configuration (inherit config)
                                             (channels %system-channels)
                                             (authorized-keys (cons (plain-file
                                                                     "systole-signing-key.pub"
                                                                     "(public-key 
 (ecc 
  (curve Ed25519)
  (q #4EB06D3040B7AC87026B998030225A9E14DE383FFAD6FAAA87F0B9267321E7BC#)
  )
 )")
                                                                    (guix-configuration-authorized-keys
                                                                     config)))))))
   (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
   (swap-devices (list (swap-space
                         (target (uuid
                                  "58922844-3a00-461d-be53-2c13db2eacbf")))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "7A3E-392A"
                                        'fat32))
                          (type "vfat"))
                        (file-system
                          (mount-point "/")
                          (device (uuid
                                   "fb977e55-9372-4f37-9637-686428fae36a"
                                   'ext4))
                          (type "ext4")) %base-file-systems))))
