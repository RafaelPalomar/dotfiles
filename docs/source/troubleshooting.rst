===============
Troubleshooting
===============

Service Issues
==============

.. code-block:: bash

    # Check shepherd (service manager) status
    herd status

    # Restart specific service
    herd restart <service-name>

    # View service logs
    herd log <service-name>

Build Failures
==============

.. code-block:: bash

    # Clear build cache
    guix gc

    # Update channels
    guix pull --channels=channels.scm

    # Check for substitute availability
    guix weather <package-name>

Emacs Daemon Issues
===================

.. code-block:: bash

    # Check daemon status
    herd status emacs

    # View logs
    herd log emacs

    # Restart daemon
    herd restart emacs

    # Kill and restart manually
    emacsclient -e '(kill-emacs)'
    emacs --daemon
