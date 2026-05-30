Entelequia Dotfiles System
===========================

Welcome to the Entelequia documentation!

This is a **GNU Guix-based declarative dotfiles system** called "entelequia".
It manages complete system configurations for multiple machines (desktop and laptop)
using Guix's functional package management and home-environment system. The entire
system state is declared in Scheme code, making it fully reproducible.

.. note::
   This documentation is also available as a generated ``CLAUDE.md`` file in the
   repository root for AI assistant compatibility.

Contents
--------

.. toctree::
   :maxdepth: 2
   :caption: Documentation:

   overview
   architecture
   configuration
   commands
   packages
   security
   operational-guide
   testing
   troubleshooting

.. toctree::
   :maxdepth: 2
   :caption: Key & Secret Management:

   secrets
   keys-inventory
   adr/index
   gpg
   bitwarden-rofi

Indices and Tables
==================

* :ref:`genindex`
* :ref:`search`
