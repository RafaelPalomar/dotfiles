(define-module (entelequia deploy hopper))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (add-to-load-path
   (canonicalize-path
    (string-append (dirname (current-filename)) "/../.."))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines hopper))

;;; Guix deployment specification for hopper (Dell XPS 13, Intel iGPU)
;;;
;;; Usage:
;;;   guix time-machine -C ~/.dotfiles/channels-lock.scm -- \
;;;     deploy -L . entelequia/deploy/hopper.scm
;;;
;;; Pre-requisites:
;;;   1. Hopper installed via systole-installer-hopper-*.iso
;;;   2. UUIDs in entelequia/system/machines/hopper.scm replaced with
;;;      actual values from `blkid` on the freshly-installed hopper.
;;;   3. host-key field below filled in via:
;;;        ssh-keyscan -t ed25519 192.168.88.245
;;;   4. host-name updated to match hopper's actual IP (or .local mDNS).
;;;
;;; SSH note: deploy uses the gpg-agent SSH socket on curie; the
;;; authorising private half is the [A] subkey
;;; 8CD379CC5C542D09F0DBC9B63CB146150266C7CE on rafael's master
;;; (managed via manage-deploy-keys.sh).

(define hopper-deployment
  (list
   (machine
    (operating-system hopper-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "192.168.88.245")
      (system "x86_64-linux")
      (user "root")
      (port 2222)                            ; entelequia hardened-ssh-service port
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGrnxsl9saA461MR2f+VsOHy38USa+62kMDRch+xJSQf")
      (allow-downgrades? #t))))))

hopper-deployment
