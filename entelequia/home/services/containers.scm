(define-module (entelequia home services containers)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages containers)
  #:use-module (guix gexp)
  #:export (home-containers-service-type))

;;; Home services for containers (podman and distrobox)
;;;
;;; Based on RDE containers feature, adapted for entelequia.
;;; Provides configuration files for rootless podman and distrobox.

(define (home-containers-profile-service config)
  "Add podman and distrobox packages to home profile."
  (list podman podman-compose distrobox))

(define (home-containers-configuration-service config)
  "Generate container configuration files."
  (define username "rafael")  ; TODO: Make this configurable

  (define init-hook
    (computed-file
     "container-init-hook"
     (with-imported-modules '((guix build utils))
       #~(let ()
           (use-modules (guix build utils))
           (call-with-output-file #$output
             (lambda (port)
               (display
                (string-append
                 "#!/bin/sh\n"
                 "\n"
                 "# Preserve environment variables in sudo (if sudoers.d exists)\n"
                 "if [ -d /etc/sudoers.d ]; then\n"
                 "  echo 'Defaults:" #$username " env_keep+=ZDOTDIR' >> /etc/sudoers.d/distrobox 2>/dev/null || true\n"
                 "  echo 'Defaults:" #$username " env_keep+=TERMINFO' >> /etc/sudoers.d/distrobox 2>/dev/null || true\n"
                 "  echo 'Defaults:" #$username " env_keep+=TERMINFO_DIRS' >> /etc/sudoers.d/distrobox 2>/dev/null || true\n"
                 "  echo 'Defaults:" #$username " env_keep+=CONTAINER_ID' >> /etc/sudoers.d/distrobox 2>/dev/null || true\n"
                 "  chmod 0440 /etc/sudoers.d/distrobox 2>/dev/null || true\n"
                 "fi\n"
                 "\n"
                 "# Create container-specific profile setup script\n"
                 "# Use zz- prefix to ensure it runs LAST after all other profile.d scripts\n"
                 "cat > /etc/profile.d/zz-container-guix-isolation.sh << 'PROFILE_EOF'\n"
                 "# Isolate container from host Guix environment to prevent library conflicts\n"
                 "# Only set these if we're in a distrobox container\n"
                 "if [ -n \"$CONTAINER_ID\" ]; then\n"
                 "  # Remove Guix paths from PATH to use container-native tools\n"
                 "  export PATH=$(echo $PATH | tr ':' '\\n' | grep -v '/gnu/store\\|/.guix-\\|/.local/share/guix' | tr '\\n' ':' | sed 's/:$//')\n"
                 "  \n"
                 "  # Clear library paths to prevent loading host libraries\n"
                 "  unset LD_LIBRARY_PATH\n"
                 "  unset LIBRARY_PATH\n"
                 "  \n"
                 "  # Clear GIO/GLib paths to use container-native modules\n"
                 "  unset GIO_EXTRA_MODULES\n"
                 "  unset GI_TYPELIB_PATH\n"
                 "  \n"
                 "  # Clear Guix-specific variables\n"
                 "  unset GUIX_PROFILE\n"
                 "  unset GUIX_ENVIRONMENT\n"
                 "  unset GUIX_LOCPATH\n"
                 "  \n"
                 "  # Set XDG paths to use container-native applications\n"
                 "  export XDG_DATA_DIRS=\"/usr/local/share:/usr/share:$HOME/.local/share\"\n"
                 "  \n"
                 "  # Customize prompt to show container name\n"
                 "  # Extract container name from CONTAINER_ID (format: distrobox-name)\n"
                 "  CONTAINER_NAME=$(echo \"$CONTAINER_ID\" | sed 's/^distrobox-//')\n"
                 "  # Set custom prompt with container name\n"
                 "  if [ -n \"$BASH_VERSION\" ]; then\n"
                 "    # Set a complete prompt if PS1 is empty or just the container prefix\n"
                 "    if [ -z \"$PS1\" ] || [ \"$PS1\" = \"\\[\\033[1;36m\\][📦 $CONTAINER_NAME]\\[\\033[0m\\] \" ]; then\n"
                 "      export PS1=\"\\[\\033[1;36m\\][📦 $CONTAINER_NAME]\\[\\033[0m\\] \\u@\\h \\w\\$ \"\n"
                 "    else\n"
                 "      export PS1=\"\\[\\033[1;36m\\][📦 $CONTAINER_NAME]\\[\\033[0m\\] $PS1\"\n"
                 "    fi\n"
                 "  elif [ -n \"$ZSH_VERSION\" ]; then\n"
                 "    export PS1=\"%F{cyan}[📦 $CONTAINER_NAME]%f %n@%m %~%# \"\n"
                 "  else\n"
                 "    export PS1=\"[$CONTAINER_NAME] \\u@\\h:\\w\\$ \"\n"
                 "  fi\n"
                 "fi\n"
                 "PROFILE_EOF\n"
                 "chmod 644 /etc/profile.d/zz-container-guix-isolation.sh 2>/dev/null || true\n"
                 "\n"
                 "# Also add to BASH_ENV so it runs for non-interactive shells\n"
                 "cat > /etc/profile.d/zz-distrobox-bashenv.sh << 'BASHENV_EOF'\n"
                 "export BASH_ENV=\"/etc/profile.d/zz-container-guix-isolation.sh\"\n"
                 "BASHENV_EOF\n"
                 "chmod 644 /etc/profile.d/zz-distrobox-bashenv.sh 2>/dev/null || true\n"
                 "\n"
                 "# Source at the END of /etc/bash.bashrc (runs last for interactive shells)\n"
                 "if [ -f /etc/bash.bashrc ]; then\n"
                 "  echo '' >> /etc/bash.bashrc 2>/dev/null || true\n"
                 "  echo '# Distrobox: Source container isolation script LAST' >> /etc/bash.bashrc 2>/dev/null || true\n"
                 "  echo '[ -f /etc/profile.d/zz-container-guix-isolation.sh ] && . /etc/profile.d/zz-container-guix-isolation.sh' >> /etc/bash.bashrc 2>/dev/null || true\n"
                 "fi\n"
                 "\n"
                 "exit 0\n")
                port)))
           (chmod #$output #o755)))))

  `(;; Podman registries configuration
    ("containers/registries.conf"
     ,(plain-file
       "registries.conf"
       "unqualified-search-registries = ['docker.io', 'registry.fedoraproject.org', 'registry.access.redhat.com', 'registry.centos.org']\n"))

    ;; Podman storage configuration (overlay for ext4)
    ("containers/storage.conf"
     ,(plain-file
       "storage.conf"
       "[storage]\ndriver = \"overlay\"\n"))

    ;; Podman security policy
    ("containers/policy.json"
     ,(plain-file
       "policy.json"
       "{\"default\": [{\"type\": \"insecureAcceptAnything\"}]}\n"))

    ;; Distrobox configuration
    ("distrobox/distrobox.conf"
     ,(mixed-text-file
       "distrobox.conf"
       ;; Sync hostname with host for tramp/vterm compatibility
       "container_hostname=\"$(uname -n)\"\n"
       ;; Mount /gnu read-only so init hook can run, then isolation cleans environment\n"
       "container_additional_volumes=\"/gnu:/gnu:ro\"\n"
       ;; Init hook preserves env vars and isolates container from Guix libraries\n"
       "container_init_hook=\"" init-hook "\"\n"))))

(define home-containers-service-type
  (service-type
   (name 'home-containers)
   (description "Configure podman and distrobox for rootless containers.")
   (extensions
    (list (service-extension
           home-profile-service-type
           home-containers-profile-service)
          (service-extension
           home-xdg-configuration-files-service-type
           home-containers-configuration-service)))
   (default-value #f)))
