(define-module (entelequia deploy lovelace))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (add-to-load-path
   (canonicalize-path
    (string-append (dirname (current-filename)) "/../.."))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines lovelace))

;;; Guix deployment specification for lovelace (192.168.88.46)
;;;
;;; Usage:
;;;   guix time-machine -C channels.scm -- deploy -L . entelequia/deploy/lovelace.scm
;;;
;;; Pre-requisites:
;;;   1. Base Guix System installed on the server (see migration plan)
;;;   2. sops GPG private key deployed to /var/lib/sops on the server
;;;   3. Obtain host key with: ssh-keyscan -t ed25519 192.168.88.46
;;;      (update host-key below before first deploy)
;;;
;;; SSH port is 2222 (hardened by security-hardening layer).

(define lovelace-deployment
  (list
   (machine
    (operating-system lovelace-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "192.168.88.46")
      (system "x86_64-linux")
      (user "root")
      (port 2222)
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII+aryHkWazuE9/b3vaincnAU3w5Uow0q5rYe+Yyg/jQ")
      (allow-downgrades? #f))))))

lovelace-deployment
