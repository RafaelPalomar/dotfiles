(define-module (entelequia home machines curie-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles networking)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services chromium)
  #:use-module (entelequia home services tailscale-work)
  #:use-module (guix-hermes packages hermes)
  #:use-module (guix-hermes services hermes)
  #:use-module (btv tailscale)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles))

;;; curie home environment — rafael
;;;
;;; Laptop, AMD GPU.  Full bspwm desktop + laptop services (batsignal) +
;;; userspace tailscaled for the work tailnet (side-by-side with the
;;; system-level personal tailscaled).

(home-environment
 (packages
  (append (base-home-packages #:gpu-type 'amd)
          (development-home-packages)
          ;; GNS3 stays on einstein where there's no hermes-agent — its
          ;; gns3-server pulls python-aiohttp 3.11.18 which collides
          ;; with hermes-agent's discord-py (aiohttp 3.13.4) in the
          ;; profile.  Curie keeps wireshark, tcpdump, nmap, autossh,
          ;; winbox; if you need GNS3 here, hop on einstein.
          (networking-home-packages #:gns3? #f)
          email-home-packages
          documentation-home-packages
          (gaming-home-packages)
          (list tailscaled
                hermes-agent)))
 (services
  (append
   (common-home-services)
   (desktop-home-services)
   (laptop-home-services)
   (chromium-home-services)
   (list (service home-tailscale-work-service-type)
         ;; Hermes Agent gateway as a user shepherd service.  Secrets
         ;; (OPENAI_API_KEY, ANTHROPIC_API_KEY, TELEGRAM_BOT_TOKEN, …)
         ;; live in ~/.hermes/secrets.env — sourced if present, ignored
         ;; if missing, so the daemon doesn't fail to start before
         ;; the file is populated.
         (service home-hermes-service-type
                  (home-hermes-configuration
                   (environment-file
                    (string-append (getenv "HOME") "/.hermes/secrets.env"))))
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
