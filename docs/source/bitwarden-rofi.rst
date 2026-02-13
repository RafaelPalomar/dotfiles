==================================
Bitwarden Password Manager + Rofi
==================================

Overview
========

Bitwarden is integrated into the desktop environment using ``rbw`` (Rust Bitwarden CLI) with ``rofi-rbw`` as the rofi frontend. This provides keyboard-driven access to passwords, TOTP codes, and secure notes with autotype and clipboard functionality.

**Key Features:**

- Cloud-synced password vault
- TOTP (2FA) code generation
- Autotype for secure credential entry
- Clipboard auto-clear after 15 seconds
- Integrates with existing pinentry-rofi
- Auto-lock after 1 hour of inactivity

Architecture
============

**Components:**

- ``rbw``: Rust-based Bitwarden CLI with stateful agent (rbw-agent)
- ``rofi-rbw``: Python-based rofi frontend for rbw
- ``xdotool``: Provides autotype functionality
- ``xclip``: Handles clipboard operations
- ``pinentry-rofi``: Password prompt interface (shared with GPG)

**Why rbw over official bitwarden-cli?**

- Stateful agent maintains session (no re-login required)
- Better pinentry integration
- Faster for desktop use
- Auto-sync with configurable interval

Installation
============

The packages are installed via Guix Home configuration:

.. code-block:: scheme

    ;; In entelequia/home/services/desktop.scm
    rbw          ;; Bitwarden CLI with agent
    xdotool      ;; For autotype functionality
    xclip        ;; For clipboard operations

The ``rofi-rbw`` frontend is installed via pip:

.. code-block:: bash

    pip install --user rofi-rbw

First-Time Setup
================

Run the setup script:

.. code-block:: bash

    ~/.local/bin/bitwarden-setup.sh

This will:

1. Prompt for your Bitwarden email via rofi
2. Configure rbw with optimal settings
3. Prompt for master password via pinentry-rofi
4. Log into Bitwarden
5. Perform initial vault sync

**Manual Setup (Alternative):**

.. code-block:: bash

    # Configure rbw
    rbw config set email your-email@example.com
    rbw config set lock_timeout 3600
    rbw config set sync_interval 3600
    rbw config set pinentry pinentry-rofi

    # Login and sync
    rbw login
    rbw sync

Usage
=====

Keyboard Shortcuts
------------------

**Primary shortcuts (sxhkd):**

- ``super + p`` - Open Bitwarden password picker
- ``super + shift + p`` - Quick TOTP code copy

**Within rofi-rbw picker:**

- ``Alt + 1`` - Autotype username + password (most secure)
- ``Alt + 2`` - Autotype username only
- ``Alt + 3`` / ``Enter`` - Autotype password
- ``Alt + u`` - Copy username to clipboard
- ``Alt + p`` - Copy password to clipboard
- ``Alt + t`` - Copy TOTP code to clipboard

Typical Workflow
----------------

1. Press ``super + p`` to open password picker
2. Type to search for credential (e.g., "github")
3. Press ``Alt + 1`` to autotype username and password (most secure)
4. Alternatively, press ``Alt + p`` to copy password to clipboard

For TOTP codes:

1. Press ``super + shift + p``
2. Select item with TOTP
3. Code is copied to clipboard and auto-cleared after 15 seconds

Management Commands
-------------------

.. code-block:: bash

    # Unlock vault manually
    rbw unlock

    # Lock vault manually
    rbw lock

    # Sync with Bitwarden server
    rbw sync

    # List all vault items
    rbw list

    # Get specific password (outputs to stdout)
    rbw get "Item Name"

    # Get password for specific field
    rbw get "Item Name" --field username

    # Generate new password
    rbw generate

    # Add new item
    rbw add "Item Name"

    # Edit existing item
    rbw edit "Item Name"

    # Check if vault is unlocked
    rbw unlocked

Configuration
=============

rbw Configuration
-----------------

**File:** ``~/.config/rbw/config.json``

.. code-block:: json

    {
      "email": "your-bitwarden-email@example.com",
      "lock_timeout": 3600,
      "sync_interval": 3600,
      "pinentry": "pinentry-rofi"
    }

**Settings:**

- ``lock_timeout``: 3600 seconds (1 hour) - matches GPG agent cache
- ``sync_interval``: 3600 seconds - auto-sync every hour
- ``pinentry``: Uses pinentry-rofi for consistent UI

rofi-rbw Configuration
----------------------

**File:** ``~/.config/rofi-rbw/config``

.. code-block:: ini

    # rofi-rbw configuration
    action = type
    target = password
    prompt = Bitwarden
    clear-after = 15
    selector-args = -dmenu -i -p
    keybindings = Alt+1:type:username-password,Alt+2:type:username,Alt+3:type:password,Alt+u:copy:username,Alt+p:copy:password,Alt+t:copy:totp

**Settings:**

- ``action = type``: Default to autotype (more secure than clipboard)
- ``target = password``: Default field to autotype
- ``clear-after = 15``: Clear clipboard after 15 seconds
- ``keybindings``: Custom keybindings for quick actions

Theming
-------

rofi-rbw automatically inherits the rofi configuration from ``~/.config/rofi/config.rasi``, including pywal dynamic colors. No additional theming configuration needed.

Security Features
=================

Auto-Lock
---------

The vault automatically locks after 1 hour (3600 seconds) of inactivity. The lock timeout is configurable in ``~/.config/rbw/config.json``.

**Manual lock:** Press ``super + shift + l`` or run ``rbw lock``

Clipboard Auto-Clear
--------------------

When copying passwords or TOTP codes to clipboard, they are automatically cleared after 15 seconds. This prevents password leakage through clipboard history.

Autotype Preference
-------------------

Autotype (``Alt + 1``, ``Alt + 2``, ``Alt + 3``) is more secure than clipboard copy because:

- No clipboard history
- No clipboard snooping
- Direct keyboard simulation
- Clears immediately after typing

**Always prefer autotype over clipboard copy.**

Pinentry Integration
--------------------

rbw uses the same ``pinentry-rofi`` as GPG for password prompts, providing:

- Consistent visual appearance
- Secure password entry (masked input)
- X11/D-Bus integration
- Follows existing security patterns

Session Management
------------------

The rbw-agent maintains the unlocked session and handles:

- Auto-lock after timeout
- Auto-lock on suspend/hibernate
- Session persistence across reboots (requires re-unlock)

Troubleshooting
===============

rofi-rbw Not Found
------------------

Ensure ``~/.local/bin`` is in your PATH:

.. code-block:: bash

    echo $PATH | grep ".local/bin"

If not found, rebuild Guix home configuration:

.. code-block:: bash

    cd ~/.dotfiles
    sudo guix time-machine -C channels.scm -- system reconfigure entelequia/system/machines/einstein.scm

rbw-agent Not Starting
----------------------

Check if the agent is running:

.. code-block:: bash

    pgrep rbw-agent

If not running, try unlocking manually:

.. code-block:: bash

    rbw unlock

Check logs:

.. code-block:: bash

    journalctl --user -u rbw-agent

Pinentry Not Appearing
----------------------

Verify pinentry-rofi is in PATH:

.. code-block:: bash

    which pinentry-rofi

Check rbw configuration:

.. code-block:: bash

    cat ~/.config/rbw/config.json | grep pinentry

Sync Issues
-----------

Manually sync:

.. code-block:: bash

    rbw sync

Check network connectivity:

.. code-block:: bash

    ping vault.bitwarden.com

Verify API URL in config (should be default for Bitwarden.com):

.. code-block:: bash

    rbw config show

Empty Vault
-----------

If rofi-rbw shows no items:

1. Check if logged in: ``rbw unlocked``
2. Force sync: ``rbw sync``
3. List items directly: ``rbw list``
4. Check Bitwarden web vault has items

Clipboard Not Clearing
----------------------

Verify rofi-rbw configuration:

.. code-block:: bash

    cat ~/.config/rofi-rbw/config | grep clear-after

Should show ``clear-after = 15``.

Keybinding Not Working
----------------------

Reload sxhkd:

.. code-block:: bash

    pkill -USR1 -x sxhkd

Or use the keybinding: ``alt + shift + r``

Verify keybinding is defined:

.. code-block:: bash

    grep "super + p" ~/.config/sxhkd/sxhkdrc

Integration with Existing Setup
================================

Complementary to password-store
--------------------------------

Bitwarden complements rather than replaces ``password-store``:

- **password-store**: Used for GPG-encrypted local secrets (OAuth2 tokens, API keys)
- **Bitwarden**: Used for cloud-synced passwords, 2FA codes, secure notes

Both systems coexist peacefully and serve different use cases.

Email OAuth2 Workflow
---------------------

The existing OAuth2 email setup continues to use ``password-store``:

.. code-block:: bash

    # OAuth2 tokens remain in password-store
    ~/.password-store/email/ntnu.no
    ~/.password-store/email/uio.no

Bitwarden can store the Microsoft account passwords, but the OAuth2 refresh tokens remain in password-store for mbsync/msmtp integration.

Rofi Theme Consistency
----------------------

rofi-rbw automatically inherits:

- Pywal dynamic colors (``~/.cache/wal/colors-rofi.rasi``)
- Font settings (Iosevka 12)
- Icon theme (Papirus-Dark)
- Window dimensions and styling

No additional configuration needed for visual consistency.

Advanced Usage
==============

Custom Bitwarden Server
------------------------

To use a self-hosted Bitwarden server:

.. code-block:: bash

    rbw config set base_url https://your-server.com
    rbw config set identity_url https://your-server.com/identity

Backup and Restore
------------------

Export vault (encrypted):

.. code-block:: bash

    # Login to Bitwarden web vault
    # Settings -> Tools -> Export Vault
    # Choose "Encrypted .json" format

The encrypted export is safe to store in your dotfiles or backup system.

Scripting with rbw
------------------

Example: Get password for automated scripts:

.. code-block:: bash

    #!/usr/bin/env bash
    PASSWORD=$(rbw get "Service Name" --field password)
    some-command --password "$PASSWORD"

**Security note:** Only use in trusted scripts. Prefer prompting interactively when possible.

Polybar Status Module (Future Enhancement)
-------------------------------------------

A polybar module can show vault lock status:

.. code-block:: bash

    #!/usr/bin/env bash
    # ~/.local/bin/bitwarden-status.sh

    if ! command -v rbw &> /dev/null; then
        echo ""
        exit 0
    fi

    if pgrep -x rbw-agent > /dev/null 2>&1; then
        if rbw unlocked 2>/dev/null; then
            echo " "  # Unlocked
        else
            echo " "  # Locked
        fi
    else
        echo ""  # Not running
    fi

Add to polybar configuration:

.. code-block:: ini

    [module/bitwarden]
    type = custom/script
    exec = ~/.local/bin/bitwarden-status.sh
    interval = 10
    click-left = rofi-rbw

Updating
========

Update rbw
-----------

rbw is managed by Guix:

.. code-block:: bash

    guix pull
    cd ~/.dotfiles
    sudo guix time-machine -C channels.scm -- system reconfigure entelequia/system/machines/einstein.scm

Update rofi-rbw
---------------

rofi-rbw is managed by pip:

.. code-block:: bash

    pip install --user --upgrade rofi-rbw

Packaging rofi-rbw (Future)
----------------------------

For full Guix integration, rofi-rbw can be packaged:

Create ``entelequia/packages/rofi-rbw.scm`` following the pattern of other Python packages. This would eliminate the pip dependency and provide declarative management.

See Also
========

- `rbw GitHub <https://github.com/doy/rbw>`_
- `rofi-rbw GitHub <https://github.com/fdw/rofi-rbw>`_
- `Bitwarden CLI Help <https://bitwarden.com/help/cli/>`_
- GPG Configuration: ``gpg.rst``
- Security Hardening: ``security.rst``
- Common Commands: ``commands.rst``
