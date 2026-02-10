(define-module (entelequia home services shell)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-shell-service-type))

;;; Shell home service
;;;
;;; Consolidated bash configuration extracted from systems/desktop.scm.
;;; Includes email authentication aliases, 3D Slicer setup, and direnv integration.

(define home-shell-service-type
  (service-type (name 'home-shell)
                (description "Shell configuration with bash, direnv, and Slicer setup")
                (extensions
                 (list
                  ;; Slicer profile setup in bash_profile
                  (service-extension
                   home-bash-service-type
                   (lambda (config)
                     (home-bash-extension
                      (bash-profile
                       (list (plain-file "setup-slicer-profile-qt5"
                                         "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-qt5 5")
                             (plain-file "setup-slicer-profile-qt6"
                                         "~/.local/bin/setup-guix-slicer-profile.sh ~/.slicer-guix-profile-qt6 6"))))))

                  ;; Bash configuration with aliases and direnv
                  (service-extension
                   home-bash-service-type
                   (lambda (config)
                     (home-bash-configuration
                      (aliases '(("auth-email-ntnu" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/ntnu.no --authorize --authflow localhostauthcode --email rafael.palomar@ntnu.no")
                                 ("auth-email-uio" . "mutt_oauth2.py --provider microsoft --client-id 08162f7c-0fd2-4200-a84a-f25a4db0b584 --client-secret  TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82 ~/.password-store/email/uio.no --authorize --authflow localhostauthcode --email rafael.palomar@ous-research.no")
                                 ("mbsync-all" . "guix shell cyrus-sasl-xoauth2 -L ~/dotfiles -- mbsync -a")))
                      (bashrc (list (plain-file "bashrc-direnv"
                                                "# if direnv is installed, run the hook
if hash direnv 2> /dev/null; then
    # get the shell name
    tmp_shell=\"$(basename \"$SHELL\")\"
    # add the hook
    eval \"$(direnv hook ${tmp_shell})\"
fi"))))))

                  ;; Environment variables for Slicer and PATH
                  (service-extension
                   home-environment-variables-service-type
                   (lambda (config)
                     '(("PATH" . "$HOME/.local/bin:$PATH")
                       ("SLICER_GUIX_PROFILE" . "$HOME/.slicer-guix-profile-qt6"))))))
                (default-value #f)))
