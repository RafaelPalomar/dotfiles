(define-module (entelequia system machines baroja)
  #:use-module (entelequia lib records)
  #:use-module (entelequia lib helpers)
  #:use-module (entelequia system layers base)
  #:use-module (entelequia system layers desktop-base)
  #:use-module (entelequia system lib common-packages)
  #:use-module (sops packages sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (guix gexp)               ; local-file (sops secrets file)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:export (baroja-os))

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
   (machine-type 'laptop)
   ;; Sandy Bridge X220 with aging cooling runs hot at idle under the default
   ;; 'performance AC profile (turbo + performance EPP).  'cool caps frequency
   ;; on AC to keep temps in check; repaste/declog is the durable hardware fix.
   (cpu-ac-profile 'cool)))

;;; Baroja-specific packages
;;
;; intel-microcode: CPU errata + vulnerability mitigations.
;; tlp / powertop: laptop power tooling (TLP service enabled via
;; machine-type 'laptop in the base layer).
;; lm-sensors: `sensors' CLI for thermal/voltage probes.

;; intel-microcode comes via gpu-driver-packages ('intel) in desktop-base;
;; thermald via the (intel, laptop) conditional in the base layer.
(define baroja-extra-packages
  (append
   (specifications->packages '("tlp" "powertop" "lm-sensors"))
   (specifications->packages workstation-packages)))

;;; Home environment for rafael lives in
;;; entelequia/home/machines/baroja-rafael.scm and is deployed
;;; independently via `guix home reconfigure' (alias `home-reconfigure').

;;; Baroja-specific services (slim/podman/librewolf/thermald all come from
;;; the layers now)

;;; SOPS encrypted secrets file (in git, encrypted).  Decrypted at boot by the
;;; Baroja SOPS key in /root/.gnupg (passwordless, generated on the host,
;;; fingerprint 7BFF3457442479BAD396C122AE6968E8FC6C9607 — see .sops.yaml).
(define %sops-baroja
  (local-file "../../../sops/baroja.yaml"))

(define baroja-services
  (list
   ;; sops-guix: decrypt rafael's OpenRouter key to /run/secrets/ at boot.
   ;; openrouter/rafael -> /run/secrets/openrouter/rafael (owner rafael, 0400),
   ;; read by the alpha launcher (rafael's home service) — same wiring as curie.
   (service sops-secrets-service-type
            (sops-service-configuration
             (sops sops)
             (gnupg-home "/root/.gnupg")
             (secrets
              (list (sops-secret (key '("openrouter" "rafael"))
                                 (file %sops-baroja)
                                 (user "rafael")
                                 (permissions #o400))))))))

(define baroja-os
  (operating-system
   (inherit (make-desktop-base-os baroja-config
                                  #:extra-packages baroja-extra-packages
                                  #:extra-services baroja-services
                                  ;; Laptop power tweaks for the X220 (Sandy Bridge)
                                  ;;   i915.enable_fbc=1     FBC — supported + stable here
                                  ;;   pcie_aspm=force       Aggressive PCIe ASPM
                                  ;;   mem_sleep_default=deep  S3 deep sleep (X220 supports it)
                                  ;; (No i915.enable_psr — PSR is Haswell+.)
                                  ;; Predictable iface names kept (no net.ifnames=0) to match
                                  ;; the installed state on a remote box.
                                  #:extra-kernel-arguments '("i915.enable_fbc=1"
                                                             "pcie_aspm=force"
                                                             "mem_sleep_default=deep")
                                  #:firewall-trusted-subnets '("192.168.88.0/24")
                                  #:ssh-authorized-keys
                                  `(("root" ,(plain-file
                                              "baroja-deploy.pub"
                                              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHLL6b/8zk5+uIj/0WxYLMAYI+3y7ZEJPsjF9jXYgR0R openpgp:0xC2B1C020")))))

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
