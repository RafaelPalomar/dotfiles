(define-module (entelequia system lib common-services)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages games)  ; For steam-devices-udev-rules
  #:use-module (guix gexp)
  #:export (gamepad-udev-rules-service
            bluetooth-input-config-service
            zram-service
            networkmanager-polkit-service
            gnutls-tls-config-service
            ntnu-vpn-connection-service
            bolt-service-type))

;;; Common service definitions shared between desktop systems
;;;
;;; This module extracts the duplicate service definitions
;;; from einstein and curie, providing a single source of truth.

;;; Udev rules for game controllers (PS4, PS5, Xbox, etc.)

(define gamepad-udev-rules-service
  (udev-rules-service 'steam-devices steam-devices-udev-rules
                      #:groups '("input")))

;;; Bluetooth input.conf — allow non-bonded HID devices (e.g. PS5 DualSense)
;;;
;;; /etc/bluetooth is a symlink to a read-only store directory, so we cannot
;;; use etc-service-type to add files to it.  Instead, create a writable
;;; /run/bluetooth/ overlay during activation: symlink main.conf from the
;;; existing store path and write input.conf as a real file, then redirect
;;; the /etc/bluetooth symlink to /run/bluetooth/.

(define bluetooth-input-config-service
  (simple-service 'bluetooth-input-conf
                  activation-service-type
                  #~(let* ((bt-link  "/etc/bluetooth")
                           (bt-store (readlink bt-link))
                           (new-dir  "/run/bluetooth"))
                      (mkdir-p new-dir)
                      ;; Symlink main.conf from the store directory
                      (let ((dst (string-append new-dir "/main.conf")))
                        (unless (file-exists? dst)
                          (symlink (string-append bt-store "/main.conf") dst)))
                      ;; Write input.conf (overwrite on each activation)
                      (call-with-output-file (string-append new-dir "/input.conf")
                        (lambda (port)
                          (display "[General]\nClassicBondedOnly = false\n" port)))
                      ;; Redirect /etc/bluetooth to our overlay
                      (false-if-exception (delete-file bt-link))
                      (symlink new-dir bt-link))))

;;; Note: polkit-wheel-service is provided by (gnu services desktop)
;;; and is used directly in base.scm - no custom definition needed
;;;
;;; Note: bluetooth-service-type and libvirt-service-type are already
;;; configured in base.scm - no need for separate definitions here

;;; GnuTLS TLS version configuration service
;;; Disables TLSv1.0, TLSv1.1, and TLSv1.3 for OpenConnect compatibility with NTNU VPN

(define gnutls-config
  (plain-file "config"
              "[overrides]
disabled-version = tls1.0
disabled-version = tls1.1
disabled-version = tls1.3
"))

(define gnutls-tls-config-service
  (simple-service 'gnutls-config
                  etc-service-type
                  (list `("gnutls/config" ,gnutls-config))))

;;; NetworkManager PolicyKit service
;;; Allows wheel group users to manage network connections, create VPNs, etc.

(define networkmanager-polkit-rules
  (file-union
   "networkmanager-polkit"
   `(("share/polkit-1/rules.d/60-networkmanager.rules"
      ,(plain-file "60-networkmanager.rules"
                   "polkit.addRule(function(action, subject) {
    if (action.id.indexOf(\"org.freedesktop.NetworkManager.\") == 0 &&
        subject.isInGroup(\"wheel\")) {
        return polkit.Result.YES;
    }
});
")))))

(define networkmanager-polkit-service
  (simple-service 'networkmanager-polkit
                  polkit-service-type
                  (list networkmanager-polkit-rules)))

;;; NTNU VPN — declarative NetworkManager connection profile
;;;
;;; NM's keyfile plugin reads system-wide VPN connections from
;;; /etc/NetworkManager/system-connections/ and refuses to load files
;;; that are not mode 0600.  etc-service-type produces world-readable
;;; entries, so we install via an activation gexp and chmod explicitly.
;;;
;;; UUID is pinned for a stable identity across reconfigures.  Bring it
;;; up from a graphical session (nm-applet tray, or `nmcli con up NTNU`)
;;; so NetworkManager invokes nm-openconnect-auth-dialog, whose embedded
;;; webkit renders the Feide SAML login.  As of 2026-07 NTNU's ASA
;;; offers embedded-webview SAML (single-sign-on-v2) ONLY — the old
;;; `ntnu-vpn-up` / `openconnect --external-browser` path now fails with
;;; "No SSO handler" and no longer works.  Deliberately NO authtype /
;;; username keys: those forced a non-interactive password attempt that
;;; blocked the SAML auth-dialog.  Auth stays on a human keystroke
;;; (Feide 2FA) — no automated login, no stored secret.

(define ntnu-vpn-connection
  (plain-file "NTNU.nmconnection"
              "[connection]
id=NTNU
uuid=0dea1bee-f1ee-41ca-bade-c0ffee0001aa
type=vpn
autoconnect=false

[vpn]
gateway=vpn.ntnu.no
protocol=anyconnect
enable_csd_trojan=no
pem_passphrase_fsid=no
service-type=org.freedesktop.NetworkManager.openconnect
useragent=AnyConnect Linux
authgroup=DefaultWEBVPNGroup
reported_os=linux-64

[ipv4]
method=auto
never-default=false

[ipv6]
method=auto
"))

(define ntnu-vpn-connection-service
  (simple-service 'ntnu-vpn-connection
                  activation-service-type
                  #~(let ((dir "/etc/NetworkManager/system-connections")
                          (dst "/etc/NetworkManager/system-connections/NTNU.nmconnection")
                          (src #$ntnu-vpn-connection))
                      (mkdir-p dir)
                      (false-if-exception (delete-file dst))
                      (copy-file src dst)
                      (chmod dst #o600))))

;;; zram compressed swap service

(define* (zram-service #:key (size-mb 8192))
  "Create zram compressed swap device.
   SIZE-MB: zram device size in MiB (default 8192 = 8GB).
   Uses zstd compression and priority 100 (higher than disk swap)."
  (list
   ;; Load zram kernel module
   (simple-service 'zram-module
                   kernel-module-loader-service-type
                   '("zram"))

   ;; Configure zram via modprobe.d
   (simple-service 'zram-config
                   etc-service-type
                   (list `("modprobe.d/zram.conf"
                          ,(plain-file "zram.conf"
                                      "options zram num_devices=1"))))

   ;; Shepherd service to initialize and activate zram swap
   (simple-service 'zram-swap
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "zram compressed swap device")
                     (provision '(zram-swap))
                     (requirement '(udev))
                     (start #~(lambda ()
                                (let ((zram-dev "/dev/zram0")
                                      (size-bytes (* #$size-mb 1024 1024)))
                                  ;; Wait for device to appear
                                  (let loop ((tries 10))
                                    (when (and (> tries 0)
                                              (not (file-exists? "/sys/block/zram0")))
                                      (sleep 1)
                                      (loop (- tries 1))))
                                  ;; Set compression algorithm to zstd
                                  (call-with-output-file "/sys/block/zram0/comp_algorithm"
                                    (lambda (port) (display "zstd" port)))
                                  ;; Set disk size
                                  (call-with-output-file "/sys/block/zram0/disksize"
                                    (lambda (port) (display size-bytes port)))
                                  ;; Initialize as swap
                                  (system* #$(file-append util-linux "/sbin/mkswap")
                                          "-L" "zram0" zram-dev)
                                  ;; Activate swap with priority 100
                                  (system* #$(file-append util-linux "/sbin/swapon")
                                          "-p" "100" zram-dev)
                                  #t)))
                     (stop #~(lambda ()
                               (system* #$(file-append util-linux "/sbin/swapoff")
                                       "/dev/zram0")
                               #f))
                     (respawn? #f))))))

;;; Thunderbolt device manager (bolt / boltd)
;;;
;;; Guix upstream has no `bolt' service, so we wire one ourselves.  This
;;; integrates the four pieces boltd needs:
;;;
;;;   - shepherd : run libexec/boltd as a long-lived daemon so it watches
;;;                Thunderbolt uevents and re-authorizes *enrolled* devices
;;;                automatically on every connect/boot.
;;;   - dbus     : boltd owns org.freedesktop.bolt on the system bus.
;;;   - polkit   : lets a wheel user run `boltctl enroll' without sudo.
;;;   - udev     : installs 90-bolt.rules (device tagging).
;;;
;;; The kernel default Thunderbolt security level is `user', meaning every
;;; new device must be explicitly authorized.  boltd persists per-UUID
;;; enrollment in /var/lib/boltd, so once the dock is enrolled it comes up
;;; on its own — without auto-authorizing arbitrary (potentially hostile)
;;; Thunderbolt hardware, unlike a blanket auto-authorize udev rule.
;;;
;;; One-time enrollment after this service is live (wheel user, no sudo):
;;;   boltctl enroll <uuid>     # uuid from `boltctl list'
;;; The dock is then authorized automatically on every subsequent plug.

(define bolt-shepherd-service
  (shepherd-service
   (documentation "Thunderbolt device manager (boltd).")
   (provision '(bolt))
   (requirement '(dbus-system udev))
   (start #~(make-forkexec-constructor
             (list #$(file-append bolt "/libexec/boltd"))))
   (stop #~(make-kill-destructor))
   (respawn? #t)))

(define bolt-service-type
  (service-type
   (name 'bolt)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (const (list bolt-shepherd-service)))
          (service-extension dbus-root-service-type
                             (const (list bolt)))
          (service-extension polkit-service-type
                             (const (list bolt)))
          (service-extension udev-service-type
                             (const (list bolt)))
          (service-extension profile-service-type
                             (const (list bolt)))))
   (default-value #f)
   (description "Run @command{boltd}, the Thunderbolt device manager, and
install its udev rules, D-Bus configuration and polkit policy.  Enrolled
Thunderbolt devices (e.g. a docking station) are re-authorized
automatically on connect.")))
