(define-module (entelequia home machines curie-rafael)
  #:use-module (entelequia home profiles base)
  #:use-module (entelequia home profiles development)
  #:use-module (entelequia home profiles role)
  #:use-module (entelequia home profiles documentation)
  #:use-module (entelequia home profiles gaming)
  #:use-module (entelequia home profiles networking)
  #:use-module (entelequia packages games)   ; gog-barony (curie-only title)
  #:use-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services chromium)
  #:use-module (entelequia home services alpha)
  #:use-module (entelequia home services forage)
  #:use-module (guix-hermes packages hermes)
  #:use-module (guix-hermes services hermes)
  #:use-module (btv tailscale)
  #:use-module (gnu packages games)       ; fheroes2 (HoMM2 engine; assets are user-supplied)
  #:use-module (guix gexp)                ; plain-file (netheroes2 .desktop launcher)
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
          (home-role-packages 'work)
          documentation-home-packages
          (gaming-home-packages)
          ;; curie-only game.  Deliberately NOT in the shared
          ;; gaming-home-packages list (which the kids' homes
          ;; alucard-leandro / hopper-adrian also consume), so Barony
          ;; never reaches the children's profiles.
          (list gog-barony
                fheroes2         ; HoMM2 single-player / hot-seat engine
                netheroes2       ; online-multiplayer HoMM2 (heroes2.online); reuses fheroes2 assets
                tailscaled
                hermes-agent)))
 (services
  (append
   (common-home-services #:email-aliases? #t #:slicer-aliases? #t #:claude-skills? #t)
   (desktop-home-services)
   (laptop-home-services)
   (chromium-home-services)
   ;; Work role: ~/.config/entelequia/role marker + the userspace work
   ;; tailscaled (curie-only; #:work-tailnet? #t).  #:manage-mail? stays
   ;; default-off, so ~/.mbsyncrc still comes from the blanket dotfiles/
   ;; copy until the coordinated mail cut (ADR-0005, role.scm header).
   (home-role-services 'work #:work-tailnet? #t)
   ;; alpha — rafael's personal pi agent (pi + the `alpha' wrapper that reads
   ;; the OpenRouter key from /run/secrets/openrouter/rafael, sops-decrypted).
   (alpha-home-service)
   ;; forage — the queen's forager-dispatch entrypoint (Stage 1 of the colony):
   ;; a `forage' command that spawns a one-shot isolated forager (Haiku, no PKS).
   (forage-home-service)
   ;; App-menu entry for netheroes2 — online multiplayer (fheroes2 fork).  We
   ;; stripped the fork's own .desktop in the package to avoid colliding with
   ;; fheroes2, so provide one here.  Binary is on PATH via the home profile.
   ;; fheroes2 ships its own .desktop, so it already appears in the menu.
   (list
    (simple-service
     'netheroes2-desktop-launcher
     home-files-service-type
     (list
      (list ".local/share/applications/netheroes2.desktop"
            (plain-file "netheroes2.desktop"
             (string-append
              "[Desktop Entry]\n"
              "Version=1.0\n"
              "Type=Application\n"
              "Name=Heroes II Online (netheroes2)\n"
              "GenericName=Turn-based strategy\n"
              "Comment=Online multiplayer HoMM II via heroes2.online "
              "(fheroes2 fork; uses the fheroes2 assets)\n"
              "Exec=netheroes2\n"
              "Icon=fheroes2\n"
              "Terminal=false\n"
              "Categories=Game;StrategyGame;\n"
              "Keywords=homm;heroes;might;magic;online;multiplayer;\n"))))))
   (list
         ;; Hermes Agent gateway as a user shepherd service.  Secrets
         ;; (OPENAI_API_KEY, ANTHROPIC_API_KEY, TELEGRAM_BOT_TOKEN, …)
         ;; live in ~/.hermes/secrets.env — sourced if present, ignored
         ;; if missing, so the daemon doesn't fail to start before
         ;; the file is populated.
         ;;
         ;; Onboarding workflow:
         ;;   1. hermes login    (Nous Portal OAuth in a browser)
         ;;   2. hermes setup    (interactive config wizard — writes
         ;;                       ~/.hermes/config.yaml)
         ;;   3. pass insert hermes/openai-key     (one per provider)
         ;;      pass insert hermes/telegram-bot-token  ...
         ;;   4. hermes-pass-sync  (refresh ~/.hermes/secrets.env from
         ;;                         pass — see dotfiles/.local/bin/)
         ;;   5. herd restart hermes
         (service home-hermes-service-type
                  (home-hermes-configuration
                   (environment-file
                    (string-append (getenv "HOME") "/.hermes/secrets.env"))))
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories '("../../../dotfiles"))))))))
