# Entelequia GPG Key Generation - Master Guide

**Complete step-by-step guide for offline key generation**

---

## Overview

This guide walks you through generating your GPG master key and subkeys **offline** using the provided utility scripts. After generation, you'll have a secure cryptographic infrastructure for:

- Git commit signing
- SSH authentication (via GPG)
- Email encryption
- Multi-machine setup (einstein + curie)
- IronKey master key storage

---

## Prerequisites Checklist

Before starting, verify these are complete:

### Infrastructure (Already Done ✓)

- [x] Shell configured with GPG environment variables
- [x] GPG agent service with SSH support
- [x] Directory structure created (~/.keys)
- [x] Utility scripts installed in ~/.local/bin
- [x] Documentation created

### Physical Preparation (Your Responsibility)

- [ ] **IronKey formatted and ready**
      - Hardware password set
      - Sufficient space (at least 100MB)
- [ ] **Safe/lockbox available** for printed revocation certificate
- [ ] **Strong passphrase prepared** (6+ random words, diceware method)
      - Example: "correct horse battery staple fluffy penguin golden"
- [ ] **Network disconnected** (optional but recommended for max security)
- [ ] **Time allocated**: 1-2 hours undisturbed

---

## Phase 1: Offline Key Generation

**Location:** Air-gapped machine or einstein with network disabled

### Step 1.1: Prepare Environment

```bash
# Optional: Disconnect network for maximum security
nmcli networking off

# Verify directory structure
ls -la ~/.keys/gpg/

# Verify scripts are available
which gpg-generate-master.sh
```

### Step 1.2: Generate Master Key

```bash
# Run master key generation script
gpg-generate-master.sh
```

**Interactive prompts:**
- Full name: `Rafael Palomar`
- Email: Your primary email address
- Comment: Optional (can leave blank)

**In GPG interactive mode:**
1. Select: `(8) RSA (set your own capabilities)`
2. Toggle OFF: Sign, Encrypt
3. Keep: Certify only
4. Key size: `4096`
5. Expiration: `0` (does not expire)
6. Passphrase: Enter your strong passphrase

**Save the Key ID shown at the end!** Example: `0xABCDEF1234567890`

### Step 1.3: Generate Subkeys

```bash
# Replace KEYID with your actual key ID
export KEYID="0xYOURKEYID"

gpg-generate-subkeys.sh $KEYID
```

**This will create THREE subkeys interactively:**

1. **Signing Subkey [S]** - for git commits
   - Select: `(4) RSA (sign only)`
   - Key size: `4096`
   - Expiration: `2y`

2. **Encryption Subkey [E]** - for email/files
   - Select: `(6) RSA (encrypt only)`
   - Key size: `4096`
   - Expiration: `2y`

3. **Authentication Subkey [A]** - for SSH
   - Select: `(8) RSA (set your own capabilities)`
   - Toggle: Authenticate only
   - Key size: `4096`
   - Expiration: `2y`

### Step 1.4: Generate Revocation Certificate

**CRITICAL STEP**

```bash
gpg-generate-revocation.sh $KEYID
```

**After generation:**
1. **PRINT** the revocation certificate (plaintext version)
2. **STORE** printed copy in safe/lockbox
3. Delete plaintext when script prompts (keeps encrypted version)

**Label the printed paper:** "GPG Revocation Certificate - DO NOT LOSE"

---

## Phase 2: Backup to IronKey

### Step 2.1: Connect IronKey

1. Physically connect IronKey
2. Enter hardware password
3. Verify mount point

```bash
# Check IronKey is mounted
ls -la /media/$USER/
# Note the mount point (e.g., /media/rafael/IRONKEY)
```

### Step 2.2: Backup Master Key

```bash
# Replace with your IronKey mount point
gpg-backup-to-ironkey.sh $KEYID /media/$USER/IRONKEY
```

**This backs up:**
- Encrypted master key
- Full GNUPGHOME backup
- Revocation certificate (encrypted)
- Public key
- Key fingerprint
- Restore instructions

### Step 2.3: Verify IronKey Contents

```bash
ls -la /media/$USER/IRONKEY/gpg/
```

**Should contain:**
- `master-key.asc.gpg`
- `master-key-backup-YYYYMMDD.tar.gpg`
- `revocation-cert.asc.gpg`
- `public-key.asc`
- `fingerprint.txt`

### Step 2.4: Safely Unmount IronKey

```bash
umount /media/$USER/IRONKEY
# Physically remove IronKey
# Store in safe/lockbox
```

---

## Phase 3: Create Subkey-Only Keyring

**CRITICAL SECURITY STEP:** Remove master key secret from daily-use machine

```bash
gpg-create-subkey-only.sh $KEYID
```

**What this does:**
1. Exports subkeys only
2. Backs up current keyring (safety backup)
3. Deletes master key secret
4. Re-imports subkeys only

**Verify master key is offline:**

```bash
gpg --list-secret-keys $KEYID
```

**Look for:** `sec#` (the `#` means master key secret is NOT present - correct!)

---

## Phase 4: Configure System

### Step 4.1: Apply Guix Configuration

```bash
# Reconfigure home environment with updated shell config
cd ~/.dotfiles
guix home reconfigure entelequia/systems/desktop.scm

# Restart GPG agent
gpgconf --kill gpg-agent
gpgconf --launch gpg-agent

# Logout/login or source bashrc
source ~/.bashrc
```

### Step 4.2: Verify Environment

```bash
# Check environment variables
echo $GPG_TTY
echo $SSH_AUTH_SOCK

# Verify GPG agent
gpgconf --check-programs

# Check SSH keys available via GPG
ssh-add -L
```

### Step 4.3: Configure Git

```bash
gpg-setup-git.sh $KEYID your-email@example.com
```

### Step 4.4: Extract SSH Public Key

```bash
# Export SSH public key from GPG
gpg --export-ssh-key $KEYID > ~/.keys/ssh/gpg-auth-key.pub

# View the key
cat ~/.keys/ssh/gpg-auth-key.pub
```

---

## Phase 5: Distribute to Other Machines

### Step 5.1: Prepare Distribution Package

```bash
# On current machine (with subkeys)
gpg-distribute-subkeys.sh $KEYID /tmp/gpg-transfer
```

### Step 5.2: Transfer to Target Machine

**Option A: Encrypted USB**
```bash
# Copy to encrypted USB
cp -r /tmp/gpg-transfer /media/usb-encrypted/

# On target machine (einstein or curie):
cd /media/usb-encrypted/gpg-transfer
./import-subkeys.sh

# Securely delete from USB
shred -u -n 3 subkeys-*.gpg
```

**Option B: Direct Transfer (Tailscale/SSH)**
```bash
# From source machine:
cd /tmp
tar czf - gpg-transfer | ssh target-machine "cd /tmp && tar xzf -"

# On target machine:
cd /tmp/gpg-transfer
./import-subkeys.sh
cd .. && rm -rf gpg-transfer
```

### Step 5.3: Configure Target Machine

On einstein/curie after importing:

```bash
# Configure Git
gpg-setup-git.sh $KEYID

# Extract SSH key
gpg --export-ssh-key $KEYID > ~/.keys/ssh/gpg-auth-key.pub
```

---

## Phase 6: External Service Configuration

### Step 6.1: Add SSH Key to GitHub

```bash
# Display SSH public key
cat ~/.keys/ssh/gpg-auth-key.pub
```

1. Copy the output
2. Go to: https://github.com/settings/keys
3. Click "New SSH key"
4. Title: "GPG Auth Subkey - Einstein" (or Curie)
5. Paste key
6. Save

**Test:**
```bash
ssh -T git@github.com
# Should see: "Hi RafaelPalomar! You've successfully authenticated..."
```

### Step 6.2: Upload GPG Public Key to GitHub

```bash
# Display public key
cat ~/.keys/gpg/public/public-key.asc
```

1. Go to: https://github.com/settings/keys (GPG Keys section)
2. Click "New GPG key"
3. Paste entire public key block
4. Save

**Commits will now show "Verified" badge on GitHub!**

### Step 6.3: Upload to Keyservers (Optional)

```bash
gpg --keyserver keys.openpgp.org --send-keys $KEYID
```

---

## Phase 7: Verification and Testing

### Step 7.1: Run Verification Script

```bash
gpg-verify-setup.sh $KEYID
```

**Should show all checks passing** ✓

### Step 7.2: Test Git Signing

```bash
cd ~/test-repo  # or any git repo
git commit --allow-empty -m "Test GPG signing"
git log --show-signature

# Should show: "Good signature from Rafael Palomar <your-email>"
```

### Step 7.3: Test SSH Authentication

```bash
# Test GitHub
ssh -T git@github.com

# Test between machines
ssh einstein  # or curie
```

### Step 7.4: Test Encryption

```bash
echo "test message" | gpg --encrypt --recipient $KEYID | gpg --decrypt
# Should decrypt successfully
```

---

## Phase 8: Security Finalization

### Step 8.1: Update Documentation

Fill in actual values in `~/.keys/README.md`:

```bash
nano ~/.keys/README.md
# Fill in:
# - Key ID
# - Fingerprint
# - Creation Date
# - Subkey expiration dates
# - Revocation cert location
```

### Step 8.2: Set Calendar Reminders

- **Subkey Expiration - 1 month:** Reminder to extend expiration
- **Monthly:** Verify IronKey is accessible
- **Annually:** Test full key restoration

### Step 8.3: Create Backup Schedule

```bash
# Weekly subkey backup (automated)
# Add to cron or create a script

cat > ~/.local/bin/weekly-gpg-backup.sh <<'EOF'
#!/bin/bash
DATE=$(date +%Y%m%d)
tar czf - ~/.gnupg | gpg --symmetric --cipher-algo AES256 \
    --output ~/.keys/gpg/subkeys-backup/subkeys-$(hostname)-$DATE.tar.gpg
# Keep only last 4 backups
cd ~/.keys/gpg/subkeys-backup
ls -t subkeys-$(hostname)-*.tar.gpg | tail -n +5 | xargs rm -f
EOF

chmod +x ~/.local/bin/weekly-gpg-backup.sh
```

---

## Quick Reference

### Common Commands

```bash
# List keys (verify master offline - shows sec#)
gpg --list-secret-keys

# Show key capabilities
gpg --list-keys --keyid-format LONG --with-subkey-fingerprints $KEYID

# Export SSH public key
gpg --export-ssh-key $KEYID

# Test GPG signing
echo "test" | gpg --clearsign

# Restart GPG agent
gpgconf --kill gpg-agent
gpgconf --launch gpg-agent

# Verify setup
gpg-verify-setup.sh $KEYID
```

### Script Inventory

Located in `~/.local/bin/`:

- `gpg-generate-master.sh` - Generate master key
- `gpg-generate-subkeys.sh` - Generate subkeys
- `gpg-generate-revocation.sh` - Generate revocation certificate
- `gpg-backup-to-ironkey.sh` - Backup to IronKey
- `gpg-create-subkey-only.sh` - Create subkey-only keyring
- `gpg-distribute-subkeys.sh` - Prepare distribution package
- `gpg-setup-git.sh` - Configure Git
- `gpg-verify-setup.sh` - Verify complete setup

---

## Troubleshooting

### Problem: "No secret key" errors

**Solution:**
```bash
gpg --list-secret-keys  # Verify keys are present
ssh-add -L  # Check SSH keys via GPG agent
```

### Problem: SSH not using GPG

**Solution:**
```bash
echo $SSH_AUTH_SOCK  # Should point to GPG agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
```

### Problem: Commits not signed

**Solution:**
```bash
git config --global commit.gpgsign true
git config --global user.signingkey $KEYID
```

### Problem: Can't access master key

**Solution:**
1. Connect IronKey
2. Mount IronKey
3. Import master key temporarily:
   ```bash
   gpg --decrypt /media/ironkey/gpg/master-key.asc.gpg | gpg --import
   ```

---

## Emergency Procedures

### Lost Master Key (But Have IronKey)

1. Connect IronKey
2. Decrypt and import:
   ```bash
   gpg --decrypt /media/ironkey/gpg/master-key-backup-*.tar.gpg | tar xzf - -C $HOME
   ```

### Master Key Compromised

1. Get revocation certificate (printed copy or IronKey)
2. Import and upload:
   ```bash
   gpg --import revocation-cert.asc
   gpg --send-keys $KEYID
   ```
3. Notify all contacts
4. Generate new key infrastructure

### IronKey Lost

1. Use printed revocation certificate
2. Generate new key infrastructure
3. Update all services (GitHub, servers, etc.)

---

## Completion Checklist

After following this guide, verify:

- [ ] Master key generated with Certify-only capability
- [ ] Three subkeys generated (Sign, Encrypt, Auth)
- [ ] Revocation certificate printed and stored in safe
- [ ] Master key backed up to IronKey (encrypted)
- [ ] IronKey stored in safe/lockbox
- [ ] Master key secret removed from daily-use machine (sec#)
- [ ] Guix home configuration applied
- [ ] Git configured for signing
- [ ] SSH public key extracted
- [ ] SSH key added to GitHub
- [ ] GPG public key added to GitHub
- [ ] Subkeys distributed to all machines (einstein, curie)
- [ ] All verification tests passing
- [ ] Documentation updated with actual key IDs
- [ ] Calendar reminders set

---

## Next Steps

1. Use your new GPG infrastructure daily
2. Monitor GitHub for "Verified" badges on commits
3. Test SSH authentication regularly
4. Keep backups current (weekly)
5. Review procedures before subkey expiration (2 years)

---

**See Also:**
- Main documentation: `/home/rafael/.dotfiles/docs/source/gpg.rst`
- GPG service: `/home/rafael/.dotfiles/entelequia/home/services/gpg.scm`
- Shell config: `/home/rafael/.dotfiles/entelequia/home/services/shell.scm`

**Questions?** Review the main strategy document and RST documentation.

---

**Date Created:** 2026-02-15
**System:** Entelequia (einstein + curie)
**Author:** Rafael Palomar
