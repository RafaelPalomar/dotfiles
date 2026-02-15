======================================
Cryptographic Key Infrastructure (GPG)
======================================

This system uses a **maximum security GPG architecture** with an offline master key and online subkeys. This approach provides strong security while maintaining daily usability.

Architecture Overview
=====================

Master Key + Subkeys Model
---------------------------

- **Master Key** (``[C]`` - Certification only): Kept offline on **IronKey**, used only for key management
- **Signing Subkey** (``[S]``): Git commits, documents, package signatures
- **Encryption Subkey** (``[E]``): Email, file encryption
- **Authentication Subkey** (``[A]``): SSH authentication via GPG agent

Multi-Machine Architecture
---------------------------

**Key Distribution Across Machines:**

.. list-table::
   :header-rows: 1
   :widths: 20 20 20 40

   * - Machine
     - Master Key
     - Subkeys
     - Purpose
   * - **einstein** (desktop)
     - ❌ (stub only)
     - ✅ All subkeys
     - Daily development, git signing, SSH
   * - **curie** (laptop)
     - ❌ (stub only)
     - ✅ All subkeys
     - Mobile work, presentations
   * - **IronKey**
     - ✅ Full master
     - ✅ All keys
     - Key management, offline operations

Key Benefits
------------

- ✅ Revoke/rotate subkeys without changing identity
- ✅ Compromise of daily-use key doesn't compromise master key
- ✅ Master key stays offline on encrypted backup
- ✅ Single key infrastructure for multiple purposes
- ✅ Can have different expiration policies per subkey

Directory Structure
===================

Local Machine (einstein/curie)
-------------------------------

.. code-block:: text

    ~/.keys/
    ├── gpg/
    │   ├── master/                          # Empty (master key on IronKey only)
    │   ├── public/                          # Can be version controlled in dotfiles
    │   │   ├── public-key.asc              # Public key (all subkeys)
    │   │   └── fingerprint.txt             # Key fingerprint for verification
    │   └── subkeys-backup/                  # Encrypted subkey backups
    │       └── subkeys-YYYYMMDD.tar.gpg    # Periodic encrypted backups
    ├── ssh/
    │   ├── gpg-auth-key.pub                # SSH public key extracted from GPG
    │   └── config                          # SSH config (via Guix home)
    ├── procedures/                          # Quick reference procedures
    │   ├── pre-generation-checklist.md
    │   └── post-generation-tasks.md
    └── README.md                           # Key management quick reference

IronKey Directory Structure
----------------------------

.. code-block:: text

    /media/ironkey/
    ├── gpg/
    │   ├── master-key.asc.gpg              # Master key (GPG encrypted)
    │   ├── master-key-backup-YYYYMMDD.tar.gpg  # Full GNUPGHOME backup
    │   ├── revocation-cert.asc.gpg         # CRITICAL: also print and store in safe
    │   ├── fingerprint.txt                 # Key fingerprint (plaintext OK)
    │   └── README.md                       # Key metadata (creation date, etc.)
    ├── ssh/
    │   └── infrastructure-backup/          # Backup of SSH auth subkey
    └── procedures/
        ├── key-rotation-procedure.md       # Step-by-step rotation guide
        ├── emergency-revocation.md         # Emergency procedures
        └── restore-instructions.md         # Disaster recovery

Initial Master Key Generation
==============================

**IMPORTANT**: Perform this on an air-gapped machine or bootable Linux USB for maximum security. For practical security, a clean system with network disabled is acceptable.

1. Prepare Secure Environment
------------------------------

.. code-block:: bash

    # Create key directory structure
    mkdir -p ~/.keys/gpg/{master,public,subkeys-backup}
    mkdir -p ~/.keys/ssh

    # Optional: Disconnect network for key generation
    nmcli networking off  # or physically disconnect

    # Set restrictive permissions
    chmod 700 ~/.keys
    chmod 700 ~/.keys/gpg/master

2. Generate Master Key (Certification Only)
--------------------------------------------

.. code-block:: bash

    # Use expert mode for certification-only key
    gpg --expert --full-generate-key

    # Select:
    # (8) RSA (set your own capabilities)
    # Toggle off Sign and Encrypt, keep only Certify
    # Key size: 4096 bits
    # Expiration: 0 (does not expire) - master key stays valid
    # Real name: Rafael [Your Last Name]
    # Email: your-primary-email@example.com
    # Comment: Master Certification Key (optional)

    # Use a STRONG passphrase (6+ words, diceware recommended)
    # Example: "correct horse battery staple fluffy penguin"

Record Key Information
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Get your key fingerprint
    gpg --list-keys --keyid-format LONG your-email@example.com

    # Save fingerprint to file
    gpg --fingerprint your-email@example.com | tee ~/.keys/gpg/public/fingerprint.txt

    # Export master key ID
    export KEYID="YOUR_KEY_ID_HERE"  # e.g., 0xABCDEF1234567890

3. Generate Subkeys
-------------------

Signing Subkey (for Git commits, documents)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    gpg --expert --edit-key $KEYID
    gpg> addkey
    # Select: (4) RSA (sign only)
    # Key size: 4096 bits
    # Expiration: 2y (2 years - shorter than master for rotation)
    gpg> save

Encryption Subkey (for email, files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    gpg --expert --edit-key $KEYID
    gpg> addkey
    # Select: (6) RSA (encrypt only)
    # Key size: 4096 bits
    # Expiration: 2y (2 years)
    gpg> save

Authentication Subkey (for SSH)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    gpg --expert --edit-key $KEYID
    gpg> addkey
    # Select: (8) RSA (set your own capabilities)
    # Toggle off Sign and Encrypt, toggle on Authenticate
    # Key size: 4096 bits
    # Expiration: 2y (2 years)
    gpg> save

4. Generate Revocation Certificate
-----------------------------------

**CRITICAL**: Generate and secure this immediately:

.. code-block:: bash

    # Generate revocation certificate
    gpg --output ~/.keys/gpg/master/revocation-cert.asc \
        --gen-revoke $KEYID

    # IMPORTANT STEPS:
    # 1. Print this file and store in a safe (fire-proof if possible)
    # 2. Store encrypted copy on separate media
    # 3. DO NOT lose this - it's your only way to revoke if master key is lost

    # Create encrypted backup
    gpg --symmetric --cipher-algo AES256 \
        ~/.keys/gpg/master/revocation-cert.asc

    # Verify the encrypted backup
    gpg --decrypt ~/.keys/gpg/master/revocation-cert.asc.gpg

5. Backup Master Key
--------------------

.. code-block:: bash

    # Export secret master key (encrypted with your passphrase)
    gpg --export-secret-keys --armor $KEYID > \
        ~/.keys/gpg/master/master-key.asc

    # Create encrypted backup of entire GNUPGHOME
    tar czf - ~/.gnupg | gpg --symmetric --cipher-algo AES256 \
        --output ~/.keys/gpg/master/master-key-backup.tar.gpg

    # Verify backup integrity
    gpg --decrypt ~/.keys/gpg/master/master-key-backup.tar.gpg | tar tzf - > /dev/null
    echo "Backup verified: $?"  # Should be 0

6. Export Public Key
--------------------

.. code-block:: bash

    # Export public key (shareable)
    gpg --export --armor $KEYID > ~/.keys/gpg/public/public-key.asc

    # Optionally, upload to key servers
    gpg --send-keys $KEYID

    # Or upload to specific keyserver
    gpg --keyserver keys.openpgp.org --send-keys $KEYID

7. Create Subkey-Only Keyring (Daily Use)
------------------------------------------

**Critical step**: Remove master key from daily-use keyring for security.

.. code-block:: bash

    # Export all subkeys (without master secret key)
    gpg --export-secret-subkeys $KEYID > /tmp/subkeys.gpg

    # Backup current keyring
    cp -r ~/.gnupg ~/.gnupg.backup

    # Delete secret master key from keyring
    gpg --delete-secret-keys $KEYID

    # Import subkeys back (master key secret is now absent)
    gpg --import /tmp/subkeys.gpg

    # Securely delete temporary file
    shred -u /tmp/subkeys.gpg

    # Verify: Master key should show "sec#" (# means secret is not present)
    gpg --list-secret-keys
    # Look for: sec#  rsa4096/KEYID [C] (pound sign indicates offline)

IronKey Integration
===================

The **IronKey** serves as the primary offline storage for the master key with hardware-level encryption.

Security Model
--------------

**Triple-Layer Encryption:**

1. **Hardware Layer**: IronKey's AES-256 hardware encryption
2. **GPG Layer**: Master key files encrypted with ``--symmetric --cipher-algo AES256``
3. **Key Passphrase**: Master key itself protected by strong passphrase

IronKey Setup Procedure
-----------------------

.. code-block:: bash

    # 1. Mount IronKey (will prompt for hardware password)
    # IronKey typically automounts to /media/username/IRONKEY_NAME

    # 2. Create directory structure
    mkdir -p /media/ironkey/{gpg,ssh,procedures}

    # 3. Set restrictive permissions
    chmod 700 /media/ironkey/gpg

    # 4. Create README
    cat > /media/ironkey/README.md <<'EOF'
    # IronKey - GPG Master Key Storage

    This IronKey contains the GPG master certification key.

    **CRITICAL**: Store in safe when not in use.

    Creation Date: YYYY-MM-DD
    Key Fingerprint: [TO BE FILLED]

    See: /home/rafael/.dotfiles/docs/source/gpg.rst
    EOF

Physical Security Protocol
--------------------------

1. **Storage**: Keep IronKey in safe/lockbox when not in use
2. **Usage**: Only connect for key management operations (subkey rotation, signing)
3. **Network**: Disconnect network when performing key operations
4. **Logging**: Record each use in IronKey's procedures/access-log.md

Key Storage Strategy
====================

Master Key Storage
------------------

**Primary Storage** (IronKey):

- Triple-encrypted master key
- Hardware encryption enabled
- Stored in safe when not in use
- Used only for key management

**Secondary Backup** (Optional):

1. **Encrypted external disk**: Secondary location (off-site: parent's house, bank vault)
2. **Paper backup**: QR code or printed ASCII-armored key (fire-proof safe)
3. **Encrypted cloud backup**: As last resort, triple-encrypted (GPG + VeraCrypt + provider encryption)

DO NOT
------

- ❌ Store master key on daily-use machine
- ❌ Store in unencrypted cloud storage
- ❌ Keep only one copy (redundancy is critical)
- ❌ Store revocation certificate with master key (separate locations)

Subkeys (daily use)
-------------------

- ✅ Stored in ``~/.gnupg`` (encrypted by GPG agent)
- ✅ Backed up encrypted to ``~/.keys/gpg/subkeys-backup/``
- ✅ Can be revoked/rotated without affecting identity

Daily Usage Workflow
====================

Git Commit Signing
------------------

.. code-block:: bash

    # Configure Git to use GPG (already in your Guix config)
    git config --global user.signingkey $KEYID
    git config --global commit.gpgsign true
    git config --global tag.gpgSign true

    # Sign commits automatically
    git commit -m "Your commit message"  # Automatically signed

    # Verify signatures
    git log --show-signature

Email Encryption
----------------

Your system already has GPG configured for email via ``entelequia/home/services/gpg.scm``. Ensure your email client is configured:

.. code-block:: bash

    # Export public key for correspondents
    gpg --armor --export your-email@example.com > my-public-key.asc

    # Decrypt email
    gpg --decrypt encrypted-message.asc

    # Encrypt file for someone
    gpg --encrypt --recipient their-email@example.com document.pdf

SSH Authentication via GPG
---------------------------

Enable GPG SSH Support
~~~~~~~~~~~~~~~~~~~~~~

Add to ``~/.bashrc`` or Guix shell config:

.. code-block:: bash

    # Add to shell configuration
    export GPG_TTY=$(tty)
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent

    # Add to ~/.gnupg/gpg-agent.conf (or Guix GPG service config)
    enable-ssh-support
    default-cache-ttl 3600
    max-cache-ttl 7200

Extract SSH Public Key from GPG
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Get SSH public key from authentication subkey
    gpg --export-ssh-key $KEYID > ~/.keys/ssh/gpg-auth-key.pub

    # Add to remote servers
    ssh-copy-id -i ~/.keys/ssh/gpg-auth-key.pub user@server

    # Test authentication
    ssh -v user@server  # Should use GPG key

Key Maintenance and Rotation
=============================

Extending Subkey Expiration
----------------------------

.. code-block:: bash

    # Before subkeys expire, extend expiration (requires master key)
    # 1. Import master key temporarily (from offline storage)
    gpg --import ~/.keys/gpg/master/master-key.asc

    # 2. Edit key
    gpg --edit-key $KEYID
    gpg> key 1  # Select first subkey (signing)
    gpg> expire
    # Set new expiration: 2y
    gpg> key 1  # Deselect
    gpg> key 2  # Select encryption subkey
    gpg> expire
    # Set new expiration: 2y
    gpg> key 2  # Deselect
    gpg> key 3  # Select authentication subkey
    gpg> expire
    # Set new expiration: 2y
    gpg> save

    # 3. Export updated public key
    gpg --export --armor $KEYID > ~/.keys/gpg/public/public-key.asc

    # 4. Update key servers
    gpg --send-keys $KEYID

    # 5. Remove master key again and re-import subkeys only
    # (repeat steps from "Create Subkey-Only Keyring" section)

    # 6. Backup updated keys
    tar czf - ~/.gnupg | gpg --symmetric --cipher-algo AES256 \
        --output ~/.keys/gpg/subkeys-backup/subkeys-$(date +%Y%m%d).tar.gpg

Rotating Compromised Subkeys
-----------------------------

.. code-block:: bash

    # If a subkey is compromised (requires master key)
    # 1. Import master key
    gpg --import ~/.keys/gpg/master/master-key.asc

    # 2. Revoke compromised subkey
    gpg --edit-key $KEYID
    gpg> key 1  # Select compromised subkey
    gpg> revkey
    # Reason: 1 = Key has been compromised
    gpg> save

    # 3. Generate new subkey (repeat generation steps above)

    # 4. Update public key and keyservers
    gpg --export --armor $KEYID > ~/.keys/gpg/public/public-key.asc
    gpg --send-keys $KEYID

    # 5. Remove master key from keyring again

Complete Key Revocation (Emergency)
------------------------------------

**Only if master key is compromised or permanently lost**:

.. code-block:: bash

    # Import revocation certificate
    gpg --import ~/.keys/gpg/master/revocation-cert.asc

    # Upload to key servers
    gpg --send-keys $KEYID

    # Notify contacts and generate new key infrastructure

Integration with Guix System
=============================

Your GPG configuration is already in ``entelequia/home/services/gpg.scm``. Enhance it to include:

.. code-block:: scheme

    ;; Example additions to gpg.scm or home-config.scm

    ;; Ensure GPG agent uses Rofi for passphrases (already configured)
    (simple-service 'gpg-agent-config
                    home-environment-variables-service-type
                    `(("GPG_TTY" . "$(tty)")
                      ("SSH_AUTH_SOCK" . "$(gpgconf --list-dirs agent-ssh-socket)")))

    ;; Add public key to dotfiles (version control safe)
    (simple-service 'gpg-public-key
                    home-files-service-type
                    `((".gnupg/public-key.asc" ,(local-file "~/.keys/gpg/public/public-key.asc"))))

Declarative Git Configuration
------------------------------

Add to ``home-config.scm`` or dotfiles:

.. code-block:: bash

    # In ~/.gitconfig or via Guix home-git-service
    [user]
        name = Rafael [Your Name]
        email = your-email@example.com
        signingkey = YOUR_KEY_ID_HERE
    [commit]
        gpgsign = true
    [tag]
        gpgSign = true

Best Practices
==============

Security Practices
------------------

1. **Strong Passphrases**:

   - Minimum 6 random words (diceware method)
   - Use different passphrases for master key vs. backup encryption
   - Consider using a password manager (KeePassXC) to store passphrases

2. **Master Key Protection**:

   - Never store on internet-connected device
   - Use only when needed (extending expiration, revoking subkeys)
   - Keep multiple encrypted backups in separate physical locations

3. **Revocation Certificate**:

   - Print and store in fire-proof safe
   - Keep separate from master key
   - Create multiple copies (not digital)

4. **Subkey Rotation**:

   - Set 2-year expiration on subkeys
   - Extend or rotate 1 month before expiration
   - Rotate immediately if compromise suspected

5. **Backup Verification**:

   - Test backup restoration annually
   - Verify encrypted backups decrypt correctly
   - Document restoration procedures

Operational Practices
----------------------

1. **Key Distribution**:

   - Upload public key to multiple keyservers
   - Add to GitHub/GitLab profiles
   - Include in email signatures
   - Publish fingerprint on personal website

2. **Signature Verification**:

   - Always verify signatures on received encrypted messages
   - Verify others' keys via multiple channels (TOFU - Trust On First Use)
   - Use key fingerprints, not just email addresses

3. **Backup Schedule**:

   - Weekly: Encrypted subkey backup to ``~/.keys/gpg/subkeys-backup/``
   - Monthly: Verify master key backup is accessible
   - Annually: Test full key restoration from backup

4. **Documentation**:

   - Keep key generation date and parameters in ``~/.keys/gpg/master/README.md``
   - Document all subkey rotations with dates
   - Maintain list of where public key is published

Common GPG Commands
===================

.. code-block:: bash

    # List all keys
    gpg --list-keys

    # List secret keys (show sec# for offline master)
    gpg --list-secret-keys

    # Show key fingerprint
    gpg --fingerprint $KEYID

    # Check subkey capabilities and expiration
    gpg --list-keys --keyid-format LONG --with-subkey-fingerprints $KEYID

    # Encrypt file
    gpg --encrypt --recipient your-email@example.com file.txt

    # Decrypt file
    gpg --decrypt file.txt.gpg > file.txt

    # Sign file (detached signature)
    gpg --detach-sign document.pdf
    # Verify: gpg --verify document.pdf.sig document.pdf

    # Clearsign text file
    gpg --clearsign message.txt

    # Export public key for sharing
    gpg --armor --export $KEYID > public-key.asc

    # Import someone's public key
    gpg --import their-public-key.asc

    # Sign someone's key (after verifying identity)
    gpg --sign-key their-key-id

    # Update keys from keyserver
    gpg --refresh-keys

    # Search keyserver
    gpg --search-keys email@example.com

    # Check GPG agent status
    gpgconf --check-programs
    gpgconf --list-components

    # Restart GPG agent (after config changes)
    gpgconf --kill gpg-agent
    gpg-agent --daemon

GPG Troubleshooting
===================

GPG Agent Not Working
---------------------

.. code-block:: bash

    # Check agent status
    echo "test" | gpg --clearsign

    # If fails, check GPG_TTY
    echo $GPG_TTY  # Should show /dev/pts/X

    # Restart agent
    gpgconf --kill gpg-agent
    gpgconf --launch gpg-agent

    # Check agent socket
    ls -la $(gpgconf --list-dirs agent-socket)

    # Check SSH support
    echo $SSH_AUTH_SOCK  # Should point to GPG agent socket

"No Secret Key" Errors
----------------------

.. code-block:: bash

    # Verify subkeys are present
    gpg --list-secret-keys

    # If missing, restore from backup
    gpg --import ~/.keys/gpg/subkeys-backup/subkeys.tar.gpg

    # Check keygrip (links secret keys to GPG agent)
    gpg --with-keygrip --list-secret-keys
    ls -la ~/.gnupg/private-keys-v1.d/

SSH Authentication Not Using GPG
---------------------------------

.. code-block:: bash

    # Verify environment variables
    echo $SSH_AUTH_SOCK
    echo $GPG_TTY

    # Test SSH key is available
    ssh-add -L  # Should show GPG-based key

    # If not, add to shell profile
    export GPG_TTY=$(tty)
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent

    # Verify agent has SSH support enabled
    grep enable-ssh-support ~/.gnupg/gpg-agent.conf

Passphrase Prompt Issues (Rofi)
--------------------------------

.. code-block:: bash

    # Check pinentry program (should be rofi for graphical prompts)
    gpgconf --list-components | grep pinentry

    # Update pinentry (in Guix GPG service or ~/.gnupg/gpg-agent.conf)
    # pinentry-program /path/to/pinentry-rofi

    # Test pinentry
    echo "GETPIN" | pinentry

Quick Reference Card
====================

Key Capabilities
----------------

- ``[C]`` = Certification (master key, sign other keys)
- ``[S]`` = Signing (sign commits, documents, packages)
- ``[E]`` = Encryption (encrypt emails, files)
- ``[A]`` = Authentication (SSH, authentication tokens)

Key Status Indicators
---------------------

- ``sec`` = Secret key present
- ``sec#`` = Secret key not present (stub only - GOOD for daily use)
- ``ssb`` = Secret subkey present
- ``pub`` = Public key

Expiration Policy
-----------------

- Master key: No expiration (permanent identity)
- Subkeys: 2-year expiration (rotate regularly)

Backup Locations
----------------

1. Master key: Offline encrypted storage (USB, external disk, safe)
2. Revocation cert: Printed paper in safe (separate from master key)
3. Subkeys: Encrypted backup in ``~/.keys/gpg/subkeys-backup/``
4. Public key: Version controlled in dotfiles, uploaded to keyservers

Emergency Contacts
------------------

- If master key lost: Use revocation certificate, generate new key
- If subkey compromised: Revoke subkey, generate new one with master key
- If all keys lost: Use revocation certificate, start over
