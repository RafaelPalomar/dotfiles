============
Architecture
============

Directory Structure
===================

.. code-block:: text

    /home/rafael/.dotfiles/
    ├── channels.scm              # Guix channel declarations (6 pinned channels)
    ├── emacs.org                 # Literate Emacs config (tangles to init.el)
    ├── dotfiles/                 # User dotfiles (deployed via guix-home)
    │   ├── .xsession
    │   ├── .config/              # Application configs (bspwm, sxhkd, alacritty, etc.)
    │   └── .local/bin/           # User scripts
    ├── scripts/                  # Deployment and testing scripts
    │   ├── deploy.sh
    │   ├── test-vm.sh
    │   └── validate-refactor.sh
    └── entelequia/               # Main Guix module namespace
        ├── lib/                  # Core shared utilities
        │   ├── records.scm       # machine-config record type
        │   └── helpers.scm       # GPU and package helper functions
        ├── home/                 # Home environment configuration
        │   ├── home-config.scm   # Base home environment
        │   ├── profiles/         # Package groupings
        │   │   ├── base.scm      # Essential packages
        │   │   ├── development.scm  # Development tools
        │   │   └── email.scm     # Email stack with OAuth2
        │   └── services/         # Modular home services
        │       ├── emacs.scm     # Emacs daemon service
        │       ├── desktop.scm   # Desktop services (D-Bus, PipeWire, GPG)
        │       ├── gpg.scm       # GPG agent configuration
        │       └── shell.scm     # Shell configuration
        ├── packages/             # Custom package definitions
        │   ├── emacs.scm         # Custom Emacs packages
        │   ├── fonts.scm         # Nerd fonts
        │   └── ...
        ├── system/               # System-level configuration
        │   ├── layers/           # Composable OS layers
        │   │   ├── base.scm      # Core OS foundation (~71 services)
        │   │   ├── desktop-base.scm  # Desktop layer additions
        │   │   └── server-base.scm   # Server layer (headless)
        │   ├── machines/         # Hardware-specific configurations
        │   │   ├── einstein.scm  # Desktop (NVIDIA GPU)
        │   │   └── curie.scm     # Laptop (AMD GPU)
        │   ├── lib/              # System-level shared code
        │   │   ├── common-packages.scm  # Package lists by category
        │   │   ├── common-services.scm  # Reusable service definitions
        │   │   └── security-hardening.scm  # Comprehensive security hardening
        │   └── vms/              # VM test configurations
        │       ├── test-desktop.scm  # Desktop VM for testing
        │       └── test-server.scm   # Server VM for testing
        ├── systems/              # Legacy compatibility (desktop.scm)
        └── tests/                # Test infrastructure
            └── vm-runner.scm     # VM test harness

Architecture Benefits
=====================

The layered architecture provides several key benefits:

1. **Maintainability**: Changes to shared code (packages, services) propagate to all systems automatically
2. **Extensibility**: Easy to add new machines by inheriting from base layers and providing hardware specifics
3. **Testability**: VM configurations allow safe testing before deploying to hardware
4. **Clarity**: Clear separation of concerns (base OS, desktop additions, hardware specifics)
5. **Reusability**: Composable layers can be mixed and matched (desktop-base, server-base)
6. **Parameterization**: machine-config record eliminates duplicate configuration code

Code Deduplication
------------------

The refactoring achieved ~90% reduction in duplicate code:

- Common packages extracted to 8 categories in ``common-packages.scm``
- 7+ reusable services defined in ``common-services.scm``
- GPU-specific logic centralized in ``helpers.scm``
- Home packages organized into 3 profiles (base, development, email)
