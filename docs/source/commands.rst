===============
Common Commands
===============

System Configuration
====================

.. code-block:: bash

    # Apply system configuration (run from ~/.dotfiles)
    sudo guix time-machine -C ~/.dotfiles/channels.scm -- system reconfigure entelequia/system/machines/einstein.scm  # Desktop
    sudo guix time-machine -C ~/.dotfiles/channels.scm -- system reconfigure entelequia/system/machines/curie.scm     # Laptop

    # Test build before applying (dry-run, no sudo needed)
    guix time-machine -C channels.scm -- system build -L . entelequia/system/machines/einstein.scm --dry-run

    # Test in VM before deploying to hardware
    ./scripts/test-vm.sh test-desktop  # Desktop VM test
    ./scripts/test-vm.sh test-server   # Server VM test

    # Update channels to latest pinned commits
    guix pull --channels=channels.scm

    # Search for packages
    sudo guix time-machine -C ~/.dotfiles/channels.scm -- search <package-name>

    # Check system generations
    sudo guix time-machine -C ~/.dotfiles/channels.scm -- system list-generations

    # Rollback to previous generation
    sudo guix system roll-back

    # Garbage collection (free space)
    guix gc -d 2m -F 10G  # Delete >2 month old, free 10GB

Package Management
==================

.. code-block:: bash

    # Install package temporarily (ephemeral shell)
    guix shell <package-name>

    # Add package permanently: Edit entelequia/home/home-config.scm
    # Add to the packages list around line 50-100

    # Create custom package: Add to entelequia/packages/
    # See examples: emacs.scm, fonts.scm, polybar-themes.scm

Emacs Configuration
===================

.. code-block:: bash

    # Edit literate config
    emacs emacs.org  # Tangle with C-c C-v t

    # Restart Emacs daemon
    herd restart emacs

    # Check Emacs daemon status
    herd status emacs

Development Workflow
====================

.. code-block:: bash

    # Enter development shell with dependencies
    guix shell <package1> <package2> -- bash

    # Use direnv for project-specific environments
    # .envrc files automatically activate when entering directories

    # 3D Slicer profile management
    setup-guix-slicer-profile.sh 5  # or 6 for different versions

Window Manager (bspwm)
======================

.. code-block:: bash

    # Restart bspwm
    bspc quit

    # Reload sxhkd (keybindings)
    pkill -USR1 sxhkd

    # Check polybar
    ~/.config/polybar.local/launch.sh
