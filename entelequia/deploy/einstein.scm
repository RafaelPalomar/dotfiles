(define-module (entelequia deploy einstein))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (let ((f (current-filename)))
    (when (string? f)
      (add-to-load-path
       (canonicalize-path
        (string-append (dirname f) "/../.."))))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines einstein))

;;; Guix deployment specification for einstein (NVIDIA desktop, campus).
;;;
;;; einstein has no LAN/personal-tailnet exposure from off-site; it is
;;; reached over the campus subnet at 10.54.212.26:2222 (also on the work
;;; tailnet as 100.73.45.80).  Root ssh uses the fleet deploy key
;;; (0xA08C8C2F), declared in machines/einstein.scm's #:ssh-authorized-keys.
;;;
;;; Usage:
;;;   guix time-machine -C ~/.dotfiles/channels-lock.scm -- \
;;;     deploy -L ~/.dotfiles entelequia/deploy/einstein.scm

(define einstein-deployment
  (list
   (machine
    (operating-system einstein-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "10.54.212.26")
      (system "x86_64-linux")
      (user "root")
      (port 2222)
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFbyGW26RpY9YRNcvF63yBNS6Kpe+vV0bCyfPyjjp1LW")
      (allow-downgrades? #t))))))

einstein-deployment
