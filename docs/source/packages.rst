========
Packages
========

System Components
=================

Desktop Environment Stack
--------------------------

- **Window Manager**: bspwm (binary space partition tiling)
- **Hotkey Daemon**: sxhkd (Super key prefix)
- **Compositor**: picom (transparency, shadows, animations)
- **Status Bar**: polybar (per-monitor themes)
- **Terminal**: Alacritty
- **Editor**: Emacs (lucid build, daemon mode with auto-restart)
- **Display Server**: X11 (xlibre variant for custom libinput/GPU drivers)
- **Color Scheme**: Pywal (dynamic theming from wallpapers)

Key Services
------------

- **Audio**: PipeWire (replaces PulseAudio/JACK)
- **Virtualization**: libvirt, QEMU, Podman
- **VPN**: Tailscale
- **Email**: isync + msmtp + mutt_oauth2 (OAuth2 for institutional accounts)
- **Security**: GPG agent, SSH, Fail2Ban, AIDE (file integrity)
- **Power Management**: TLP (laptop), thermald

System-Specific Features
-------------------------

Einstein (Desktop)
~~~~~~~~~~~~~~~~~~

- NVIDIA GPU support (nvidia-driver)
- SLiM display manager
- Container tools (Podman, Distrobox)
- AIDE file integrity monitoring
- More development packages

Curie (Laptop)
~~~~~~~~~~~~~~

- AMD GPU (amdgpu + mesa + xlibre)
- Lighter configuration
- Enhanced power management (TLP, thermald)
- Battery monitoring

Important Files
===============

Critical Configuration
----------------------

- ``channels.scm`` - Package source definitions (commit-pinned for reproducibility)
- ``entelequia/lib/records.scm`` - Core ``machine-config`` record type definition
- ``entelequia/lib/helpers.scm`` - GPU and package helper functions
- ``entelequia/system/layers/base.scm`` - Shared OS base (~71 services: kernel, bootloader, networking, virtualization, security)
- ``entelequia/system/layers/desktop-base.scm`` - Desktop layer (adds desktop packages to base)
- ``entelequia/system/lib/common-packages.scm`` - Shared package lists (8 categories: hardware, audio, bluetooth, X11, security, etc.)
- ``entelequia/system/lib/common-services.scm`` - Reusable service definitions (AIDE, bluetooth, libvirt, polkit, etc.)
- ``entelequia/system/lib/security-hardening.scm`` - Comprehensive security hardening (kernel, firewall, fail2ban, audit, SSH)
- ``entelequia/home/home-config.scm`` - Base home environment (packages, dotfiles service, bash config)

Machine Configurations
----------------------

- ``entelequia/system/machines/einstein.scm`` - Desktop system (NVIDIA, AIDE, Podman, more packages)
- ``entelequia/system/machines/curie.scm`` - Laptop system (AMD, TLP, thermald, power management)

Home Environment
----------------

- ``entelequia/home/profiles/base.scm`` - Essential packages (git, compression, text processing)
- ``entelequia/home/profiles/development.scm`` - Development tools (GCC, Node.js, Python, debugging)
- ``entelequia/home/profiles/email.scm`` - Email stack with OAuth2 support
- ``entelequia/home/services/emacs.scm`` - Emacs daemon service with 60+ packages
- ``entelequia/home/services/desktop.scm`` - Desktop services (D-Bus, PipeWire, GPG, polybar)
- ``entelequia/home/services/gpg.scm`` - GPG agent configuration
- ``entelequia/home/services/shell.scm`` - Shell configuration
- ``entelequia/systems/desktop.scm`` - Shared desktop home services (imported by both systems)

Test Infrastructure
-------------------

- ``entelequia/system/vms/test-desktop.scm`` - Desktop VM for testing before deployment
- ``entelequia/system/vms/test-server.scm`` - Server VM for testing
- ``entelequia/tests/vm-runner.scm`` - VM test harness
- ``scripts/test-vm.sh`` - Script to launch test VMs
- ``scripts/validate-refactor.sh`` - Validation script for module syntax
- ``scripts/deploy.sh`` - Deployment script

Custom Packages
---------------

- ``entelequia/packages/emacs.scm`` - Custom Emacs packages (denote-silo, evil-snipe, ob-mermaid, persp-projectile)
- ``entelequia/packages/fonts.scm`` - Nerd fonts (CommitMono, Iosevka, JetBrainsMono, etc.)
- ``entelequia/packages/polybar-themes.scm`` - Polybar theme collection
- ``entelequia/packages/cyrus-sasl-xoauth2.scm``, ``mutt-oauth2.scm`` - OAuth2 email authentication

User Dotfiles
-------------

- ``dotfiles/.xsession`` - X11 startup (compositor, WM, notifications)
- ``dotfiles/.config/bspwm/bspwmrc`` - Window manager config
- ``dotfiles/.config/sxhkd/sxhkdrc`` - Keybindings (Super key shortcuts)
- ``dotfiles/.config/alacritty/alacritty.toml`` - Terminal emulator
- ``dotfiles/.local/bin/`` - User scripts (wallpaper, keyboard layout, aider wrapper)

Performance Considerations
==========================

- Guix uses ``/gnu/store`` for immutable packages (can grow large, use ``guix gc`` regularly)
- Home generations accumulate (clean old ones: ``guix home delete-generations 1m``)
- Emacs daemon stays running (restart after package changes)
- PipeWire replaces PulseAudio for lower latency audio
- TLP manages laptop power automatically
- Pywal generates color schemes (may cause brief lag when changing wallpapers)
