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
- **Terminal**: Kitty
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
- ``entelequia/packages/latex.scm`` - NFR proposal LaTeX class and SciFly Sans font

NFR LaTeX Class (XeLaTeX)
~~~~~~~~~~~~~~~~~~~~~~~~~

The ``nfr`` document class provides styling for Norwegian Research Council (NFR)
grant proposals.  It requires XeLaTeX and lives under
``entelequia/packages/nfr/``.

**Guix packages defined in** ``entelequia/packages/latex.scm``:

- ``latex-nfr`` — installs ``nfr.cls`` to ``share/texmf/tex/latex/nfr/``
- ``font-sciflycore-sans`` — SciFly Sans TTF (freeware, Flyerzone)

**Font dependencies resolved from upstream Guix** (added to
``entelequia/home/profiles/development.scm``):

- ``font-dosis`` — Dosis family (headings and requirement tags)
- ``font-sil-gentium`` / ``font-sil-gentium-book`` — Gentium Plus (body text)
- ``texlive-xetex``, ``texlive-fontspec``, ``texlive-xcolor``,
  ``texlive-titlesec``, ``texlive-hyperref``, ``texlive-caption``

**Usage** — in a ``.tex`` document::

    \documentclass[12pt,a4paper]{nfr}

    \begin{document}
    \section{Section}
    Body text uses Gentium Plus. {\DosisSemiBold Heading uses Dosis.}
    \tag{REQ-01}{req:01}   % coloured boxed requirement tag
    \tagref{req:01}         % hyperlinked back-reference
    \end{document}

Compile with XeLaTeX::

    xelatex proposal.tex

A minimal smoke-test document is provided at
``entelequia/packages/nfr/test-nfr.tex``.  Run from that directory so that
the class file is on the local search path::

    cd entelequia/packages/nfr
    xelatex test-nfr.tex

A successful run produces ``test-nfr.pdf`` (≥10 KB) with lavender section
headings, Gentium body text, Dosis headings, and SciFly-Sans decorations.

.. note::

   **Gentium font naming** — ``font-sil-gentium`` v7 renamed the family from
   "Gentium Plus" (v6 and earlier) to "Gentium", and ships TTF files alongside
   WOFF web fonts in the same package.  ``nfr.cls`` uses ``Extension=.ttf`` for
   the Gentium ``\setromanfont`` declaration so that ``xdvipdfmx`` never
   resolves to the WOFF variant (which it cannot embed in PDF).

   **Dosis regular weight** — ``font-dosis`` v1.7 ships the regular weight as
   ``Dosis-Book`` (not ``Dosis-Regular``).  The ``\Dosis`` command in
   ``nfr.cls`` therefore uses the PostScript name ``Dosis-Book``.

User Dotfiles
-------------

- ``dotfiles/.xsession`` - X11 startup (compositor, WM, notifications)
- ``dotfiles/.config/bspwm/bspwmrc`` - Window manager config
- ``dotfiles/.config/sxhkd/sxhkdrc`` - Keybindings (Super key shortcuts)
- ``dotfiles/.config/kitty/kitty.conf`` - Terminal emulator
- ``dotfiles/.local/bin/`` - User scripts (wallpaper, keyboard layout, aider wrapper)

Performance Considerations
==========================

- Guix uses ``/gnu/store`` for immutable packages (can grow large, use ``guix gc`` regularly)
- Home generations accumulate (clean old ones: ``guix home delete-generations 1m``)
- Emacs daemon stays running (restart after package changes)
- PipeWire replaces PulseAudio for lower latency audio
- TLP manages laptop power automatically
- Pywal generates color schemes (may cause brief lag when changing wallpapers)
