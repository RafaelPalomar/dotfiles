(define-module (entelequia deploy alucard))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (add-to-load-path
   (canonicalize-path
    (string-append (dirname (current-filename)) "/../.."))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines alucard))

;;; Guix deployment specification for alucard.local
;;;
;;; Usage:
;;;   guix time-machine -C channels.scm -- deploy -L . entelequia/deploy/alucard.scm
;;;
;;; Requirements:
;;;   - Root SSH access to alucard.local on port 2222 (port 22 on first deploy from installer)
;;;   - Obtain the host key with:
;;;       ssh-keyscan -t ed25519 -p 2222 alucard.local
;;;   - Fill in host-key below before deploying

(define alucard-deployment
  (list
   (machine
    (operating-system alucard-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "192.168.88.41")
      (system "x86_64-linux")
      (user "root")
      (port 2222)    ; change to 2222 after first deploy (entelequia moves SSH to port 2222)
      (identity "/home/rafael/.ssh/monk-access")
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFm8sUFl89D1klI8WViVpXCKjjEa35qudJkyLSo9gncG")
      (allow-downgrades? #f))))))

alucard-deployment
