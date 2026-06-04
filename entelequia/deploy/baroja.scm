(define-module (entelequia deploy baroja))

;; Ensure the dotfiles root is on the load path regardless of invocation CWD
(eval-when (expand load eval)
  (add-to-load-path
   (canonicalize-path
    (string-append (dirname (current-filename)) "/../.."))))

(use-modules (gnu machine)
             (gnu machine ssh)
             (entelequia system machines baroja))

;;; Guix deployment specification for baroja (Lenovo ThinkPad X220, Intel)
;;;
;;; Usage:
;;;   guix time-machine -C ~/.dotfiles/channels-lock.scm -- \
;;;     deploy -L . entelequia/deploy/baroja.scm
;;;
;;; PORT BOOTSTRAP NOTE:
;;;   baroja was installed by the systole installer, whose openssh runs on
;;;   the default port 22.  The entelequia base layer's hardened sshd
;;;   listens on 2222.  So the FIRST deploy connects on 22 (below); once it
;;;   activates the entelequia config, sshd moves to 2222 -- change `port'
;;;   to 2222 for every subsequent deploy.  /etc/ssh host keys persist
;;;   across the switch, so `host-key' below stays valid.
;;;
;;; SSH note: deploy uses the gpg-agent SSH socket; the authorising private
;;; half is baroja's deploy [A] subkey (openpgp:0xC2B1C020, keygrip
;;; B577F43131AC4072CBF107FDBB00413F9A2EE9D7), managed via
;;; manage-deploy-keys.sh.  If the agent refuses to sign non-interactively,
;;; unlock once via an interactive `ssh' (pinentry-rofi, 8h cache).  Pin the
;;; key to dodge a low MaxAuthTries (now 20 fleet-wide, was 3):
;;;   SSH_AUTH_SOCK=... with IdentitiesOnly + IdentityFile systole-deploy-baroja.pub

(define baroja-deployment
  (list
   (machine
    (operating-system baroja-os)
    (environment managed-host-environment-type)
    (configuration
     (machine-ssh-configuration
      (host-name "192.168.88.117")
      (system "x86_64-linux")
      (user "root")
      (port 2222)                            ; entelequia hardened sshd (post first-deploy).
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDrs6pZEQqvyRd+f+w7/n1r263Apt1AnDtLDVWUqnNLu")
      (allow-downgrades? #t))))))

baroja-deployment
