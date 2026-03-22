Entelequia Operational Guide
=============================

Practical reference for the complete workflow: GPG key management, sops secrets,
Systole ISO generation, and ``guix deploy``. Covers USB integrity verification
at every step where data is written to removable media.

.. contents:: Contents
   :local:
   :depth: 2

----

Part 1 — Deploy Key Management
--------------------------------

Each machine gets a dedicated GPG ``[A]`` (authentication) subkey used as its SSH
deploy key. The master key lives offline on the IronKey; subkeys are managed with
``manage-deploy-keys.sh``.

Key facts:

- **Master key FP:** ``6513C7248D7BECE2EC1BD34B70350DAD507FA72F``
- **DB:** ``~/.dotfiles/dotfiles/.gnupg/deploy-keys.conf``
- **sshcontrol:** ``~/.dotfiles/dotfiles/sshcontrol``
- **Tool:** ``~/.dotfiles/dotfiles/.local/bin/manage-deploy-keys.sh``

1.1 Adding a deploy key for a new machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prerequisites: import master key from IronKey first::

    gpg --import /run/media/rafael/PRIVATE_USB/gpg/master-key.asc

Add a 2-year auth subkey::

    manage-deploy-keys.sh add <machine-name> 2y

The script prints the new subkey fingerprint and keygrip, the SSH public key to
add to the machine's ``authorized_keys``, and next steps (back up, re-strip).

1.2 Backing up the master key to IronKey
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Required after adding a subkey.** Generate a checksum before copying::

    gpg --armor --export-secret-keys 6513C7248D7BECE2EC1BD34B70350DAD507FA72F \
        | sha256sum > /tmp/master-key.sha256

Export to USB::

    gpg --armor --export-secret-keys 6513C7248D7BECE2EC1BD34B70350DAD507FA72F \
        > /run/media/rafael/PRIVATE_USB/gpg/master-key.asc

Verify USB contents match::

    sha256sum /run/media/rafael/PRIVATE_USB/gpg/master-key.asc
    # Compare with the hash in /tmp/master-key.sha256 — must match

Verify fingerprint from the USB file::

    gpg --import --dry-run /run/media/rafael/PRIVATE_USB/gpg/master-key.asc \
        2>&1 | grep "key fingerprint"
    # Must show: 6513 C724 8D7B ECE2 EC1B  D34B 7035 0DAD 507F A72F

1.3 Re-stripping the master key
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After backing up to the IronKey, remove the master private key from the machine
(keep only subkeys)::

    gpg --export-secret-subkeys 6513C7248D7BECE2EC1BD34B70350DAD507FA72F \
        > /tmp/subkeys.gpg
    gpg --delete-secret-keys 6513C7248D7BECE2EC1BD34B70350DAD507FA72F
    gpg --import /tmp/subkeys.gpg
    rm /tmp/subkeys.gpg
    gpg --list-secret-keys
    # Verify: sec# (master offline), ssb (subkeys present)

1.4 Listing and checking deploy keys
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    manage-deploy-keys.sh list           # shows name, status, expiry, SSH pubkey
    manage-deploy-keys.sh pubkey monk    # print SSH pubkey for a specific machine

Current machines in ``deploy-keys.conf``:

- **monk** — keygrip ``0FAACBA7A6363ACADF80DB1C88CE02702DFCB8BE`` (added 2026-03-09)
- **lovelace** — keygrip ``A95146C1DB21F12A646FA046E5C86C5FA7BA6802`` (added 2026-03-19)

----

Part 2 — Sops Secrets Management
----------------------------------

Each server machine has a dedicated passwordless GPG key for automated secret
decryption at boot. ``sops-guix`` decrypts ``sops/lovelace.yaml`` →
``/run/secrets/*`` at boot time.

Key files:

- ``~/.dotfiles/.sops.yaml`` — creation rules (which key encrypts which file)
- ``~/.dotfiles/sops/lovelace.yaml`` — encrypted secrets (committed to git)

Lovelace sops key:

- **Name:** ``Lovelace SOPS <lovelace-sops@palomar.no>``
- **Fingerprint:** ``0E4534607A2FA8D112176DCEDDAA34F42A158809``
- **Private key backup:** ``/run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc``

2.1 Backing up the sops private key to USB
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the key is still in your keyring (``gpg --list-secret-keys | grep lovelace-sops``)::

    gpg --armor --export-secret-keys 0E4534607A2FA8D112176DCEDDAA34F42A158809 \
        > /run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc

Generate and store a checksum alongside it::

    sha256sum /run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc \
        > /run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc.sha256
    cat /run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc.sha256

Verify fingerprint from the backed-up file::

    gpg --import --dry-run \
        /run/media/rafael/PRIVATE_USB/gpg/lovelace-sops-private.asc \
        2>&1 | grep fingerprint
    # Must show: 0E45 3460 7A2F A8D1 1217  6DCE DDAA 34F4 2A15 8809

2.2 Editing sops secrets
~~~~~~~~~~~~~~~~~~~~~~~~~

Decrypt and edit (requires the sops key in your keyring)::

    guix time-machine -C ~/.dotfiles/channels.scm -- shell sops -- \
        sops ~/.dotfiles/sops/lovelace.yaml

Re-encrypt after editing the plaintext directly::

    guix time-machine -C ~/.dotfiles/channels.scm -- shell sops -- \
        sops --encrypt --in-place ~/.dotfiles/sops/lovelace.yaml

2.3 Adding a sops key for a new machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generate a new passwordless key in a temporary home directory::

    export SOPS_GNUPGHOME="$(mktemp -d)"
    gpg --homedir "$SOPS_GNUPGHOME" --batch --generate-key <<'EOF'
    %no-protection
    Key-Type: EDDSA
    Key-Curve: ed25519
    Subkey-Type: ECDH
    Subkey-Curve: cv25519
    Name-Real: <Machine> SOPS
    Name-Email: <machine>-sops@palomar.no
    Expire-Date: 0
    EOF

    NEW_FP=$(gpg --homedir "$SOPS_GNUPGHOME" --list-keys --with-colons \
                 | grep "^fpr" | head -1 | cut -d: -f10)
    echo "Fingerprint: $NEW_FP"

Import the public key into your main keyring so sops can encrypt for it::

    gpg --homedir "$SOPS_GNUPGHOME" --export --armor | gpg --import

Back up private key to USB before the temp dir is lost::

    gpg --homedir "$SOPS_GNUPGHOME" --export-secret-keys --armor \
        > /run/media/rafael/PRIVATE_USB/gpg/<machine>-sops-private.asc
    sha256sum /run/media/rafael/PRIVATE_USB/gpg/<machine>-sops-private.asc

Then add to ``.sops.yaml`` creation rules using comma-separated fingerprints::

    # .sops.yaml
    creation_rules:
      - path_regex: sops/<machine>\.yaml$
        pgp: >-
          <MACHINE_FP>,
          6513C7248D7BECE2EC1BD34B70350DAD507FA72F

----

Part 3 — Systole Installer ISO Generation
-------------------------------------------

The Systole installer ISO is built from ``~/src/guix-systole`` using
``channels-lock.scm`` for reproducibility. The deploy key baked into the ISO
determines who can SSH to the installer.

For Lovelace use the dedicated lovelace deploy key (not monk-access).

3.1 Standard build command
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    cd ~/src/guix-systole
    ./scripts/build-installer-with-deploy.sh \
        --ssh-deploy-key "$(manage-deploy-keys.sh pubkey lovelace)" \
        --signing-key-file /etc/guix/signing-key.pub

With a pre-known SSH host key (enables strict host-key checking in the installer)::

    ./scripts/build-installer-with-deploy.sh \
        --ssh-deploy-key "$(manage-deploy-keys.sh pubkey lovelace)" \
        --signing-key-file /etc/guix/signing-key.pub \
        --host-key-file /etc/ssh/ssh_host_ed25519_key

Output: ``~/src/guix-systole/systole-installer-deploy-<TIMESTAMP>.iso``

3.2 Writing ISO to USB with integrity verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Identify the USB device first::

    lsblk

Set variables::

    ISO="$HOME/src/guix-systole/systole-installer-deploy-TIMESTAMP.iso"
    USB="/dev/sdX"   # replace X with the correct device letter

Checksum before write::

    sha256sum "$ISO" | tee "$ISO.sha256"

Write to USB::

    sudo dd if="$ISO" of="$USB" bs=4M status=progress conv=fsync
    sync

Verify USB contents match the ISO::

    ISO_SIZE=$(stat -c%s "$ISO")
    sudo dd if="$USB" bs=4M \
        count=$(( (ISO_SIZE + 4*1024*1024 - 1) / (4*1024*1024) )) \
        | sha256sum
    # Compare with the hash from the .sha256 file — must match

3.3 Using Ventoy (recommended for repeated testing)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Copy the ISO to the Ventoy partition (no ``dd`` needed; Ventoy boots ISOs directly)::

    cp "$ISO" /run/media/rafael/Ventoy/

Verify the copy::

    sha256sum "$ISO" /run/media/rafael/Ventoy/systole-installer-deploy-*.iso
    # Both hashes must be identical

----

Part 4 — Deployment Workflow
------------------------------

Full workflow from a fresh server through to a running Guix deployment.

4.1 Prerequisites (done once per machine)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Collect hardware info from the current server::

    ssh root@192.168.88.46 \
        "blkid && ip link && \
         ls /sys/firmware/efi 2>/dev/null && echo EFI || echo BIOS"

Collect borg credentials from the existing server::

    ssh root@192.168.88.46 "cat /root/.ssh/id_ed25519"      # borg SSH key
    ssh root@192.168.88.46 "cat /root/.ssh/id_ed25519.pub"  # borg SSH pubkey

Populate sops secrets (TS auth keys, DB passwords, borg, etc.)::

    guix time-machine -C ~/.dotfiles/channels.scm -- shell sops -- \
        sops ~/.dotfiles/sops/lovelace.yaml

Validate entelequia modules::

    cd ~/.dotfiles && ./scripts/validate-refactor.sh

4.2 Install base Guix System on the server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

a. Build the Systole installer ISO (see Part 3).
b. Write to USB/Ventoy and verify integrity (see Parts 3.2/3.3).
c. Boot the ISO on Lovelace; connect via SSH::

       ssh -i ~/.ssh/id_ed25519 root@192.168.88.46

   The SSH key is the lovelace ``[A]`` auth subkey managed by
   ``manage-deploy-keys.sh``. ``gpg-agent`` presents it via SSH because it is
   listed in ``sshcontrol``.

d. Run the Guix installer:

   - Reformat root partition; leave ``/data`` (btrfs, 1.9 TB) untouched.
   - Root user SSH key: lovelace deploy key pubkey.
   - DHCP on the network interface.
   - OpenSSH on port 22 (temporary; entelequia will move to 2222).

e. After reboot, collect the new SSH host key::

       ssh-keyscan -t ed25519 192.168.88.46
       # Update entelequia/deploy/lovelace.scm host-key field with this value

4.3 Pre-deploy: copy sops key to server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The sops private key must be at ``/var/lib/sops`` on the server before the first
``guix deploy`` that includes ``sops-secrets-service-type``. This is a one-time
manual step::

    ssh root@192.168.88.46 "mkdir -p /var/lib/sops"

    gpg --armor --export-secret-keys 0E4534607A2FA8D112176DCEDDAA34F42A158809 \
        | ssh root@192.168.88.46 "gpg --homedir /var/lib/sops --import"

Verify it imported correctly::

    ssh root@192.168.88.46 \
        "gpg --homedir /var/lib/sops --list-keys lovelace-sops@palomar.no"
    # Expected fingerprint: 0E4534607A2FA8D112176DCEDDAA34F42A158809

4.4 First guix deploy
~~~~~~~~~~~~~~~~~~~~~~

Use port 22 for the first deploy (before entelequia hardens SSH to port 2222).
Ensure ``entelequia/deploy/lovelace.scm`` has ``(port 22)`` and the correct
host-key from ``ssh-keyscan``::

    cd ~/.dotfiles
    guix time-machine -C channels.scm -- deploy -L . \
        entelequia/deploy/lovelace.scm

After the first deploy succeeds, update ``deploy/lovelace.scm`` to ``(port 2222)``
and commit.

4.5 Subsequent deploys
~~~~~~~~~~~~~~~~~~~~~~~

::

    cd ~/.dotfiles
    guix time-machine -C channels.scm -- deploy -L . \
        entelequia/deploy/lovelace.scm

Or using ``sync-and-deploy`` from guix-systole (handles channel version sync)::

    cd ~/src/guix-systole
    ./scripts/sync-and-deploy.sh ~/.dotfiles/entelequia/deploy/lovelace.scm

4.6 Rollback
~~~~~~~~~~~~~

::

    ssh root@192.168.88.46 -p 2222 "guix system roll-back"

4.7 Lovelace: creating the btrfs RAID10 after first boot
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This step is specific to servers with a btrfs RAID data volume.
The GUI installer only touches ``/dev/sda`` (root, EFI, swap).
After the first boot, create the RAID manually, then ``guix deploy``
as normal.

**Step 1 — Create the RAID10 array (on lovelace, as root)**

The four data drives are ``/dev/sdb``, ``/dev/sdc``, ``/dev/sdd``, ``/dev/sde``::

    mkfs.btrfs -d raid10 -m raid10 \
        /dev/sdb /dev/sdc /dev/sdd /dev/sde

This generates a new UUID.  Record it::

    blkid /dev/sdb
    # Example output: UUID="5ede0c23-b59f-4f69-830b-27a333356c8d"

**Step 2 — Update lovelace.scm if the UUID changed (on curie)**

Compare the output above with the UUID in ``entelequia/system/machines/lovelace.scm``
(the ``/data`` file-system entry).  If they differ, update the file and commit::

    # In lovelace.scm, file-systems section:
    (device (uuid "NEW-UUID-HERE" 'btrfs))

The ``options`` field already lists all four devices::

    (options "compress=zstd,space_cache=v2,device=/dev/sdb,device=/dev/sdc,device=/dev/sdd,device=/dev/sde")

No change needed there.

**Step 3 — Pre-deploy sops GPG key (on curie, see also 4.3)**

::

    ssh root@192.168.88.46 "mkdir -p /var/lib/sops"

    gpg --armor --export-secret-keys 0E4534607A2FA8D112176DCEDDAA34F42A158809 \
        | ssh root@192.168.88.46 "gpg --homedir /var/lib/sops --import"

    ssh root@192.168.88.46 \
        "gpg --homedir /var/lib/sops --list-keys lovelace-sops@palomar.no"
    # Must show: 0E4534607A2FA8D112176DCEDDAA34F42A158809

**Step 4 — First guix deploy (on curie)**

Get the SSH host key installed by the base system and update
``entelequia/deploy/lovelace.scm``::

    ssh-keyscan -t ed25519 192.168.88.46
    # Paste output into (host-key "...") in deploy/lovelace.scm
    # Set (port 22) for the first deploy (hardening will move it to 2222)

Then deploy::

    cd ~/.dotfiles
    guix time-machine -C channels-lock.scm -- deploy -L . \
        entelequia/deploy/lovelace.scm

``mount-may-fail? #t`` on the ``/data`` file-system means the btrfs volume does
not need to be mounted for the deploy to succeed — it will be picked up on the
next reboot once the kernel assembles the array using the device list in
``options``.

After the first deploy succeeds, update ``deploy/lovelace.scm`` to
``(port 2222)`` and commit.

----

Part 5 — Quick Reference
--------------------------

Deploy key operations
~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :widths: 50 50
   :header-rows: 0

   * - ``manage-deploy-keys.sh list``
     - List all deploy keys with SSH pubkeys
   * - ``manage-deploy-keys.sh add <name> 2y``
     - Add new deploy key (needs master key)
   * - ``manage-deploy-keys.sh pubkey <name>``
     - Print SSH public key for a machine
   * - ``manage-deploy-keys.sh enable <name>``
     - Enable key for SSH (add to sshcontrol)
   * - ``manage-deploy-keys.sh disable <name>``
     - Disable key

Key fingerprints
~~~~~~~~~~~~~~~~~

.. list-table::
   :widths: 40 60
   :header-rows: 0

   * - GPG master key
     - ``6513C7248D7BECE2EC1BD34B70350DAD507FA72F``
   * - Lovelace sops key
     - ``0E4534607A2FA8D112176DCEDDAA34F42A158809``
   * - Lovelace deploy keygrip
     - ``A95146C1DB21F12A646FA046E5C86C5FA7BA6802``

Network addresses
~~~~~~~~~~~~~~~~~~

.. list-table::
   :widths: 30 70
   :header-rows: 0

   * - Lovelace SSH
     - ``ssh root@192.168.88.46 -p 2222``
   * - SSH auth
     - ``gpg-agent`` SSH with lovelace ``[A]`` subkey

Common commands
~~~~~~~~~~~~~~~~

Validate modules::

    cd ~/.dotfiles && ./scripts/validate-refactor.sh

Dry-run build::

    guix time-machine -C channels.scm -- system build -L . \
        entelequia/system/machines/lovelace.scm --dry-run

USB integrity check workflow::

    sha256sum <file> | tee <file>.sha256     # before copy
    cp <file> /run/media/rafael/<usb>/       # copy
    sha256sum /run/media/rafael/<usb>/<file> # verify — must match .sha256
