(define-module (entelequia system machines baroja)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (entelequia system lib librewolf-policy)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)         ; thermald
  #:use-module (gnu services containers)
  #:use-module (gnu system accounts)
  #:use-module (xlibre)
  #:export (baroja-os))

(use-service-modules xorg containers pm)

;;; Baroja system configuration
;;;
;;; Lenovo ThinkPad X220 (4291L40), Intel Sandy Bridge.
;;;   - CPU: Intel i5-2520M (2C/4T)
;;;   - GPU: Intel HD Graphics 3000 (8086:0126) -> i915, modesetting
;;;   - Net: Intel 82579LM GbE (enp0s25) + Centrino Advanced-N 6205 WiFi
;;;          (wlp3s0, iwlwifi -- needs nonfree firmware from nonguix)
;;;   - 8 GiB RAM, single SATA disk (sda): EFI + swap + ext4 root
;;;
;;; Remote box (.117); modelled on hopper (the other deploy-managed Intel
;;; laptop) rather than curie (curie is the local AMD daily-driver).
;;; Deployed via entelequia/deploy/baroja.scm.

;;; Machine configuration

(define baroja-config
  (machine-config
   (hostname "baroja")
   (username "rafael")
   (locale "en_US.utf8")
   (timezone "Europe/Oslo")
   (keyboard (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (gpu-type 'intel)
   (machine-type 'laptop)))

;;; Intel xorg configuration
;;;
;;; modesetting (KMS) + TearFree, same approach as hopper/curie -- works
;;; well for the Sandy Bridge iGPU and avoids the legacy
;;; xlibre-video-intel DDX.

(define intel-xlibre-config
  (xlibre-configuration
   (modules (list xlibre-input-libinput))
   (drivers '("modesetting"))
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))
   (extra-config
    (list "Section \"Device\""
          "  Identifier \"Intel Graphics\""
          "  Driver \"modesetting\""
          "  Option \"TearFree\" \"true\""
          "EndSection"))))

;;; Baroja-specific packages
;;
;; intel-microcode: CPU errata + vulnerability mitigations.
;; tlp / powertop: laptop power tooling (TLP service enabled via
;; machine-type 'laptop in the base layer).
;; lm-sensors: `sensors' CLI for thermal/voltage probes.

(define baroja-extra-packages
  (append
   (specifications->packages '("intel-microcode"
                               "tlp"
                               "powertop"
                               "lm-sensors"))
   (specifications->packages curie-specific-packages)))

;;; Home environment for rafael lives in
;;; entelequia/home/machines/baroja-rafael.scm and is deployed
;;; independently via `guix home reconfigure' (alias `home-reconfigure').

;;; Baroja-specific services

(define baroja-services
  (list
   ;; Intel thermal management (base layer dropped thermald when AMD took
   ;; over; re-add for Intel laptops, as hopper does).
   (service thermald-service-type)

   ;; Rootless podman for containerization (rafael)
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subuids (list (subid-range (name "rafael"))))
             (subgids (list (subid-range (name "rafael"))))))

   ;; SLiM display manager with Intel xorg
   (service slim-service-type
            (slim-configuration
             (auto-login? #f)
             (default-user "rafael")
             (xorg-configuration intel-xlibre-config)))

   ;; Librewolf managed policy: SearXNG default + Bitwarden force-install.
   librewolf-policy-service))

(define baroja-os
  (operating-system
   (inherit (make-desktop-base-os baroja-config
                                  #:extra-packages baroja-extra-packages
                                  #:extra-services baroja-services
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file
                                              "baroja-deploy.pub"
                                              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHLL6b/8zk5+uIj/0WxYLMAYI+3y7ZEJPsjF9jXYgR0R openpgp:0xC2B1C020")))))

   ;; Intel kernel arguments + laptop power tweaks for the X220 (Sandy Bridge)
   ;;   i915.enable_fbc=1     Frame Buffer Compression -- reduces memory
   ;;                         bandwidth; supported + stable on Sandy Bridge.
   ;;   pcie_aspm=force       Aggressive PCIe Active State Power Management.
   ;;   mem_sleep_default=deep  Use S3 deep sleep (X220 supports it) rather
   ;;                         than s2idle.
   ;; (No i915.enable_psr -- Panel Self Refresh is Haswell+, not Sandy Bridge.)
   ;; Predictable iface names (enp0s25/wlp3s0) kept -- no net.ifnames=0 -- to
   ;; match the installed state and avoid surprising NetworkManager on a
   ;; remote box.
   (kernel-arguments (gpu-kernel-arguments
                      'intel
                      #:extra-args '("i915.enable_fbc=1"
                                     "pcie_aspm=force"
                                     "mem_sleep_default=deep")))

   ;; Users: rafael (admin, podman/containers).
   ;; NOTE: the systole installer created the account as "Rafael" (capital,
   ;; /home/Rafael).  Standardised here to lowercase "rafael" to match every
   ;; other entelequia machine + the baroja-rafael home file.  First deploy
   ;; creates /home/rafael; the empty /home/Rafael can be removed afterwards.
   (users (cons* (user-account
                  (name "rafael")
                  (comment "Rafael")
                  (group "users")
                  (home-directory "/home/rafael")
                  (supplementary-groups '("wheel" "netdev" "kvm" "tty" "input"
                                          "realtime" "audio" "video" "cgroup")))
                 %base-user-accounts))

   ;; Bootloader (UEFI) -- GRUB on the EFI system partition (sda1).
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))))

   ;; Swap (sda2)
   (swap-devices (list (swap-space
                        (target (uuid "28457a78-f86b-4d41-8443-8d7d360eca2d")))))

   ;; File systems (sda1 EFI + sda3 ext4 root) -- UUIDs from baroja `blkid'.
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "8273-65EC" 'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid
                                  "e1110ed6-5ec3-4b66-a6fa-8fd4242527cc"
                                  'ext4))
                         (type "ext4"))
                        %base-file-systems))))

baroja-os
