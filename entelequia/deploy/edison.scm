(define-module (entelequia deploy edison))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (add-to-load-path
   (canonicalize-path
    (string-append (dirname (current-filename)) "/../.."))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines edison))

;;; Guix deployment specification for edison (192.168.88.14)
;;;
;;; Usage:
;;;   guix time-machine -C channels.scm -- deploy -L . entelequia/deploy/edison.scm
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
;;; Host key obtained via: ssh-keyscan -t ed25519 192.168.88.14

(define edison-deployment
  (list
   (machine
    (operating-system edison-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "192.168.88.14")
      (system "x86_64-linux")
      (user "root")
      (port 2222)
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPCoqZQm/79ETjLpvc8gC6xmtnE8TEM/Q7m1F84xoL2U")
      (allow-downgrades? #t))))))

edison-deployment
