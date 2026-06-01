(define-module (entelequia home services desktop-suite)
  #:use-module (entelequia home services emacs)
  #:use-module (entelequia home services desktop)
  #:use-module (entelequia home services encrypted-usb)
  #:use-module (entelequia home services containers)
  #:use-module (entelequia home services github-sync)
  #:use-module (entelequia home services pks)
  #:use-module (entelequia packages polybar-themes)
  #:use-module (entelequia packages notebooklm-py)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:use-module (systole packages claude-skills)
  #:export (common-home-services
            desktop-home-services
            laptop-home-services))

;;; Desktop-suite home services
;;;
;;; Replaces the legacy entelequia/systems/desktop.scm (which lived under the
;;; misnamed `entelequia/systems/' directory — actually a list of HOME services,
;;; not a system).  The contents are now structured as three composable
;;; functions returning service lists:
;;;
;;;   (common-home-services)  — bash, env vars, gpg-agent, dbus, gnupg perms,
;;;                              optional claude-skills files.
;;;   (desktop-home-services) — the 6 user shepherd services + emacs + pipewire
;;;                              + containers + github-sync + pks + polybar
;;;                              themes + optional datalocker + optional slicer.
;;;   (laptop-home-services)  — batsignal.
;;;
;;; Per-(machine,user) home files in `entelequia/home/machines/' append these
;;; in whatever combination they need and add their own home-dotfiles-service.

;;; Common home services (bash, env, gpg, dbus)

(define* (common-home-services #:key
                               (email-aliases? #t)
                               (slicer-aliases? #t)
                               (claude-skills? #t)
                               (nvidia? #f))
  "Bash, environment variables, gpg-agent, dbus, gnupg perms, and (optionally)
the rafael-specific claude-skills files / slicer-profile bash hooks /
auth-email-* aliases."
  (append
   (list
    ;; Strict permissions on ~/.gnupg/ — home-dotfiles-service-type creates
    ;; the directory but may not chmod 700.
    (simple-service 'gnupg-directory-permissions
                    home-activation-service-type
                    #~(let ((gnupg-dir (string-append (getenv "HOME") "/.gnupg")))
                        (mkdir-p gnupg-dir)
                        (chmod gnupg-dir #o700)))

    ;; GnuPG agent — pinentry via rofi, long TTL so unlocks last all day.
    ;; allow-preset-passphrase enables PRESET_PASSPHRASE on the agent socket,
    ;; required so pam_gnupg (wired into SLiM's PAM stack) can forward the
    ;; login password into gpg-agent at session start, eliminating pinentry
    ;; prompts for keygrips listed in ~/.pam-gnupg.
    (service home-gpg-agent-service-type
             (home-gpg-agent-configuration
              (pinentry-program
               (file-append pinentry-rofi "/bin/pinentry-rofi"))
              (ssh-support? #t)
              (default-cache-ttl 28800)
              (max-cache-ttl 28800)
              (default-cache-ttl-ssh 28800)
              (max-cache-ttl-ssh 28800)
              (extra-content "allow-preset-passphrase\n")))

    ;; User session D-Bus.
    (service home-dbus-service-type)

    ;; Bash aliases + bashrc snippets.
    (service home-bash-service-type
             (home-bash-configuration
              (aliases
               (append
                '(("sys-reconfigure"  . "sudo guix time-machine -C ~/.dotfiles/channels-lock.scm -- system reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/system/machines/$(hostname).scm")
                  ("sys-update"       . "git -C ~/.dotfiles pull && sudo guix time-machine -C ~/.dotfiles/channels-lock.scm -- system reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/system/machines/$(hostname).scm")
                  ("home-reconfigure" . "guix time-machine -C ~/.dotfiles/channels-lock.scm -- home reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/home/machines/$(hostname)-$(whoami).scm")
                  ("home-update"      . "git -C ~/.dotfiles pull && guix time-machine -C ~/.dotfiles/channels-lock.scm -- home reconfigure -L ~/.dotfiles ~/.dotfiles/entelequia/home/machines/$(hostname)-$(whoami).scm")
                  ("mbsync-all"       . "sync-mail"))
                (if email-aliases?
                    '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id $OAUTH_CLIENT_ID --client-secret $OAUTH_CLIENT_SECRET ~/.password-store/email/ntnu.no.gpg --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                      ("auth-email-uio"  . "mutt_oauth2.py --provider microsoft --client-id $OAUTH_CLIENT_ID --client-secret $OAUTH_CLIENT_SECRET ~/.password-store/email/uio.no.gpg --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no"))
                    '())))
              (bashrc
               (append
                (list (plain-file "bashrc-direnv"
                                  "# if direnv is installed, run the hook
if hash direnv 2> /dev/null; then
    tmp_shell=\"$(basename \"$SHELL\")\"
    eval \"$(direnv hook ${tmp_shell})\"
fi")
                      (plain-file "bashrc-container-isolation"
                                  "# Source container isolation script if in a distrobox container
if [ -n \"$CONTAINER_ID\" ] && [ -f /etc/profile.d/zz-container-guix-isolation.sh ]; then
    . /etc/profile.d/zz-container-guix-isolation.sh
fi")
                      (plain-file "bashrc-gpg-agent"
                                  "# GPG TTY setup for GPG agent
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent 2>/dev/null"))
                (if email-aliases?
                    (list (plain-file "bashrc-oauth-env"
                                      "# OAuth2 client creds for `auth-email-ntnu` / `auth-email-uio` aliases.
# Values live encrypted in ~/.dotfiles/sops/rafael.yaml; decrypted once
# per shell via the user's personal GPG key (gpg-agent caches the pin).
__sops_oauth_file=\"$HOME/.dotfiles/sops/rafael.yaml\"
if [ -z \"$OAUTH_CLIENT_ID\" ] \\
   && [ -r \"$__sops_oauth_file\" ] \\
   && command -v sops >/dev/null 2>&1; then
    OAUTH_CLIENT_ID=$(sops -d --extract '[\"oauth\"][\"client_id\"]'     \"$__sops_oauth_file\" 2>/dev/null)
    OAUTH_CLIENT_SECRET=$(sops -d --extract '[\"oauth\"][\"client_secret\"]' \"$__sops_oauth_file\" 2>/dev/null)
    [ -n \"$OAUTH_CLIENT_ID\" ]     && export OAUTH_CLIENT_ID
    [ -n \"$OAUTH_CLIENT_SECRET\" ] && export OAUTH_CLIENT_SECRET
fi
unset __sops_oauth_file
"))
                    '())))))

    ;; Shared user environment variables.
    ;;
    ;; LD_LIBRARY_PATH is gated on nvidia?: it forces libglvnd ahead of
    ;; baked-in DT_RUNPATH Mesa libGL.so.1 so GLX dispatches to
    ;; libGLX_nvidia.  On Intel/AMD it's not just a no-op — it makes the
    ;; home profile shadow versions bundled via RUNPATH inside other
    ;; Guix packages (e.g. arandr's GTK 3.24.51 was linked against pango
    ;; 1.54.0 but home-profile has pango 1.56.4, and the resulting
    ;; libpangoft2/libpango symbol mismatch crashes the app).
    (simple-service 'user-env-vars
                    home-environment-variables-service-type
                    (append
                     `(("LC_COLLATE"    . "C")
                       ("VISUAL"        . "emacsclient")
                       ("EDITOR"        . "emacsclient")
                       ("PATH"          . "$HOME/.local/bin:$HOME/.npm-global/bin:$PATH")
                       ("XDG_DATA_DIRS" . "/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")
                       ;; entelequia python-playwright ships no browsers; point
                       ;; it at the home-profile ungoogled-chromium and disable
                       ;; its CDN browser download.  Runtime path string (not a
                       ;; store ref) so this adds no closure to non-chromium
                       ;; machines.
                       ("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" . "1")
                       ("PLAYWRIGHT_CHROMIUM_EXECUTABLE" . "$HOME/.guix-home/profile/bin/chromium"))
                     (if nvidia?
                         '(("LD_LIBRARY_PATH" . "$HOME/.guix-home/profile/lib:/run/current-system/profile/lib"))
                         '()))))

   (if claude-skills?
       (list (simple-service 'claude-skills-files
                             home-files-service-type
                             (list `(".claude/commands/slicer.md"
                                     ,(file-append slicer-skill
                                                   "/share/claude-skills/slicer/SKILL.md"))
                                   `(".claude/commands/guix-systole-dev.md"
                                     ,(file-append guix-systole-dev-skill
                                                   "/share/claude-skills/guix-systole-dev/SKILL.md"))
                                   ;; notebooklm-py ships a proper Agent Skill
                                   ;; (frontmatter), so link it under skills/,
                                   ;; not commands/.
                                   `(".claude/skills/notebooklm/SKILL.md"
                                     ,(file-append notebooklm-py
                                                   "/share/claude-skills/notebooklm/SKILL.md")))))
       '())

   (if slicer-aliases?
       (list (simple-service 'slicer-profile-setup
                             home-bash-service-type
                             (home-bash-extension
                              (bash-profile
                               (list (plain-file "setup-slicer-profile-qt5"
                                                 "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-5 5")
                                     (plain-file "setup-slicer-profile-qt6"
                                                 "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-6 6")))))
             (simple-service 'slicer-env-vars
                             home-environment-variables-service-type
                             `(("SLICER_GUIX_PROFILE" . "$HOME/.slicer-guix-profile-6"))))
       '())))

;;; Desktop home services (WM-adjacent: emacs, pipewire, containers, etc.)

(define* (desktop-home-services #:key (datalocker? #t))
  "Window-manager-adjacent home services: the 6 user shepherd daemons (picom,
gammastep, xautolock, udiskie, nm-applet, pass-secret-service) bundled inside
home-desktop-service-type; emacs daemon; pipewire; containers; github-sync;
pks; polybar themes file dump; and (optionally) the DataLocker auto-unlock."
  (append
   (list
    ;; The 6 user shepherd services (defined in entelequia/home/services/desktop.scm).
    (service home-desktop-service-type)

    ;; Emacs daemon with auto-restart.
    (service home-emacs-config-service-type)

    ;; PipeWire user session (replaces PulseAudio).  System provides the
    ;; udev rules + kernel paths; this is the per-user daemon.
    (service home-pipewire-service-type
             (home-pipewire-configuration (enable-pulseaudio? #t)))

    ;; Container configuration (podman + distrobox user-side bits).
    (service home-containers-service-type)

    ;; GitHub→org-mode sync (mcron-driven, every 60 minutes).
    (service home-github-sync-service-type)

    ;; PKS silo bootstrap + weekly Nextcloud-conflict scanner.
    (service home-pks-service-type)

    ;; Drop polybar theme files into ~/.config/polybar/.
    (simple-service 'polybar-themes
                    home-files-service-type
                    `((".config/polybar"
                       ,(directory-union "polybar-themes"
                                         (list polybar-themes))))))

   (if datalocker?
       (list
        ;; DataLocker Sentry ONE auto-unlock.  No auto-lock on disconnect —
        ;; lock is manual via keyboard shortcut or polybar button.
        (service home-encrypted-usb-service-type
                 (encrypted-usb-configuration
                  (name "datalocker")
                  (vendor-id "230a")
                  (model-id "1550")
                  (mount-point "UNLOCKER")
                  (unlock-command "$HOME/.local/bin/datalocker-unlock")
                  (lock-command #f)
                  (poll-interval 2))))
       '())))

;;; Laptop-only home services

(define (laptop-home-services)
  "Battery monitoring (batsignal) — laptops only."
  (list
   (service home-batsignal-service-type)))
