======================
Configuration Patterns
======================

Adding New Packages
===================

1. **System-level package (all machines)**: Edit ``entelequia/system/lib/common-packages.scm``, add to appropriate category
2. **System-level package (machine-specific)**: Edit ``entelequia/system/machines/einstein.scm`` or ``curie.scm``, add to packages list
3. **Home-level package (all machines)**: Edit ``entelequia/home/profiles/base.scm``, ``development.scm``, or ``email.scm``
4. **Home-level package (machine-specific)**: Edit ``entelequia/home/home-config.scm``, add to packages list
5. **Custom package**: Create in ``entelequia/packages/``, follow existing patterns (see ``emacs.scm``, ``fonts.scm``)

Creating New Services
=====================

1. **Home service**: Add to ``entelequia/home/services/`` (see ``emacs.scm``, ``desktop.scm``, ``gpg.scm``, ``shell.scm``)
2. **System service (shared)**: Add to ``entelequia/system/lib/common-services.scm`` for reusable services
3. **System service (layer-specific)**: Add to ``entelequia/system/layers/base.scm``, ``desktop-base.scm``, or ``server-base.scm``
4. **System service (machine-specific)**: Add to ``entelequia/system/machines/einstein.scm`` or ``curie.scm``
5. Use ``simple-service`` for basic configurations (environment variables, udev rules, etc.)

Adding a New Machine
====================

1. Create new file: ``entelequia/system/machines/new-machine.scm``
2. Define ``machine-config`` with hostname, GPU type, etc.:

   .. code-block:: scheme

       (define new-machine-config
         (machine-config
          (hostname "new-machine")
          (username "rafael")
          (gpu-type 'intel)  ; or 'nvidia, 'amd
          (machine-type 'laptop)))  ; or 'desktop

3. Inherit from appropriate base layer (desktop-base or server-base)
4. Add machine-specific file-systems, bootloader, and packages
5. Test with: ``./scripts/test-vm.sh`` (create VM config first)

Modifying Emacs Configuration
==============================

1. Edit ``emacs.org`` (literate configuration)
2. Tangle with ``C-c C-v t`` or on save (if configured)
3. Restart Emacs daemon: ``herd restart emacs``
4. Or manually: ``emacsclient -e '(kill-emacs)' && emacs --daemon``

Adding Dotfiles
===============

1. Place files in ``dotfiles/`` directory structure (mirrors home directory)
2. Run ``guix home reconfigure`` to symlink into home
3. Files are automatically deployed via ``home-dotfiles-service-type``

Module System
=============

The codebase uses Guile Scheme modules with a layered architecture:

.. code-block:: scheme

    (define-module (entelequia system layers base)
      #:use-module (gnu)
      #:use-module (entelequia lib records)
      #:use-module (entelequia system lib common-services)
      #:export (make-base-operating-system))

- **Import modules**: ``#:use-module (module name)``
- **Export definitions**: ``#:export (function-name variable-name)``
- **Use package shortcuts**: ``use-package-modules``, ``use-service-modules``

Key Module Patterns
-------------------

Layered OS Construction
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: scheme

    ;; Base layer creates foundation
    (define (make-base-operating-system config) ...)

    ;; Desktop layer adds on top
    (define (make-desktop-base-os config #:key (extra-services '()))
      (let ((base-os (make-base-operating-system config)))
        (operating-system
         (inherit base-os)
         (packages ...))))  ; Only override specific fields

    ;; Machine config adds hardware specifics
    (define einstein-base (make-desktop-base-os einstein-config))
    (define einstein-system
      (operating-system
       (inherit einstein-base)
       (bootloader ...)
       (file-systems ...)))

Using machine-config Record
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: scheme

    (use-modules (entelequia lib records))

    (define my-config
      (machine-config
       (hostname "my-machine")
       (username "user")
       (gpu-type 'nvidia)      ; 'nvidia, 'amd, or 'intel
       (machine-type 'desktop) ; 'desktop or 'laptop
       (timezone "Europe/Oslo")
       (locale "en_US.utf8")))
