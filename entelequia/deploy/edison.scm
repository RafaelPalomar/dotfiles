(define-module (entelequia deploy edison))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (let ((f (current-filename)))
    (when (string? f)
      (add-to-load-path
       (canonicalize-path
        (string-append (dirname f) "/../.."))))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines edison))

;;; Guix deployment specification for edison (Tailscale 100.121.69.14)
;;;
;;; Usage:
;;;   guix time-machine -C channels-lock.scm -- deploy -L . entelequia/deploy/edison.scm
;;;
;;; Pre-requisites:
;;;   1. Base Guix System installed (done — fresh install at 192.168.88.14)
;;;   2. sops/edison.yaml created and encrypted (see edison-services.scm for steps)
;;;   3. /var/lib/sops GPG key deployed to Edison before running containers
;;;
;;; SSH note: first deploy uses port 22 (fresh install default).
;;; Subsequent deploys use port 2222 (security-hardening changes SSH port).
;;; Update the port field below after the first successful deploy.
;;;
;;; ADDRESS: the TAILSCALE address, not the LAN one.  The LAN IP works only
;;; while the operator is at home, and the failure is quiet and confusing:
;;; `guix deploy' spends several minutes updating channels and building the
;;; system before it ever opens a socket, then dies with "Timeout connecting
;;; to 192.168.88.14".  Deployed twice from off-LAN before anyone noticed the
;;; address was the problem rather than the build.  Tailscale resolves on the
;;; LAN too, so this is strictly the better default.
;;;
;;; Host key obtained via: ssh-keyscan -t ed25519 -p 2222 100.121.69.14
;;; (same machine, so the key is unchanged from the LAN-address era)

(define edison-deployment
  (list
   (machine
    (operating-system edison-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "100.121.69.14")
      (system "x86_64-linux")
      (user "root")
      (port 2222)
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPCoqZQm/79ETjLpvc8gC6xmtnE8TEM/Q7m1F84xoL2U")
      (allow-downgrades? #t))))))

edison-deployment
