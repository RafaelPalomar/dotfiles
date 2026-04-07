(define-module (entelequia home services tailscale-work)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:export (home-tailscale-work-service-type))

;;; Tailscale Work (Userspace) Home Service
;;;
;;; Runs a SECOND tailscaled in userspace-networking mode under the user
;;; account, side-by-side with the system-level personal tailscaled.
;;; State is isolated under $XDG_STATE_HOME/tailscale-work and the daemon
;;; exposes local SOCKS5 (127.0.0.1:1055) and HTTP (127.0.0.1:1056) proxies
;;; for reaching work-tailnet hosts without touching the personal tailnet.
;;;
;;; tailscaled is taken from /run/current-system/profile/bin (always current).
;;;
;;; One-time login (interactive):
;;;   tailscale-work up --accept-dns=false

(define (home-tailscale-work-shepherd-service _)
  (list
   (shepherd-service
    (documentation "Userspace tailscaled for the work tailnet")
    (provision '(tailscale-work))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append bash-minimal "/bin/bash")
              "-c"
              "set -e; \
STATE_BASE=\"${XDG_STATE_HOME:-$HOME/.local/state}\"; \
WORKDIR=\"$STATE_BASE/tailscale-work\"; \
mkdir -p \"$WORKDIR/state\"; \
exec \"$HOME/.guix-home/profile/bin/tailscaled\" \
  --tun=userspace-networking \
  --statedir=\"$WORKDIR/state\" \
  --socket=\"$WORKDIR/tailscaled.sock\" \
  --socks5-server=127.0.0.1:1055 \
  --outbound-http-proxy-listen=127.0.0.1:1056")
        #:log-file (string-append
                    (or (getenv "XDG_STATE_HOME")
                        (string-append (getenv "HOME") "/.local/state"))
                    "/tailscale-work/tailscaled.log")
        #:environment-variables
        (cons* (string-append "PATH=" (getenv "PATH"))
               (string-append "HOME=" (getenv "HOME"))
               (default-environment-variables))))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (auto-start? #t))))

;;; The companion CLI wrapper lives in dotfiles/.local/bin/tailscale-work
;;; and is deployed via home-dotfiles-service-type.

(define home-tailscale-work-service-type
  (service-type
   (name 'home-tailscale-work)
   (description "Userspace tailscaled for a second (work) tailnet")
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-tailscale-work-shepherd-service)))
   (default-value #f)))
