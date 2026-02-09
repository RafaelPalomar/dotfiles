# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a **GNU Guix-based declarative dotfiles system** called "entelequia". It manages complete system configurations for multiple machines (desktop and laptop) using Guix's functional package management and home-environment system. The entire system state is declared in Scheme code, making it fully reproducible.

## Architecture

### Directory Structure

```
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
```

### Key Concepts

- **Declarative System**: Everything from kernel to dotfiles is declared in Scheme
- **Layered Architecture**: Base → Desktop-Base/Server-Base → Machine-specific (composable layers)
- **Parameterization**: `machine-config` record eliminates hardcoded values (hostname, GPU, timezone, etc.)
- **DRY Principle**: 90%+ code deduplication through shared libraries (common-packages, common-services)
- **Multi-System Support**: Two distinct systems (`einstein` desktop, `curie` laptop) share common base
- **Guix Channels**: 6 pinned channels provide reproducible package sources (guix, nonguix, guix-xlibre, tailscale, guix-systole, systole-artwork)
- **Home Environment**: Uses Guix Home (not GNU Stow) to manage user packages and dotfiles
- **Literate Configuration**: Emacs config written in org-mode for documentation alongside code
- **Testability**: VM configurations for pre-deployment testing

### Architecture Benefits

The layered architecture provides several key benefits:

1. **Maintainability**: Changes to shared code (packages, services) propagate to all systems automatically
2. **Extensibility**: Easy to add new machines by inheriting from base layers and providing hardware specifics
3. **Testability**: VM configurations allow safe testing before deploying to hardware
4. **Clarity**: Clear separation of concerns (base OS, desktop additions, hardware specifics)
5. **Reusability**: Composable layers can be mixed and matched (desktop-base, server-base)
6. **Parameterization**: machine-config record eliminates duplicate configuration code

**Code Deduplication**: The refactoring achieved ~90% reduction in duplicate code:
- Common packages extracted to 8 categories in `common-packages.scm`
- 7+ reusable services defined in `common-services.scm`
- GPU-specific logic centralized in `helpers.scm`
- Home packages organized into 3 profiles (base, development, email)

## Common Commands

### System Configuration

```bash
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
```

### Package Management

```bash
# Install package temporarily (ephemeral shell)
guix shell <package-name>

# Add package permanently: Edit entelequia/home/home-config.scm
# Add to the packages list around line 50-100

# Create custom package: Add to entelequia/packages/
# See examples: emacs.scm, fonts.scm, polybar-themes.scm
```

### Emacs Configuration

```bash
# Edit literate config
emacs emacs.org  # Tangle with C-c C-v t

# Restart Emacs daemon
herd restart emacs

# Check Emacs daemon status
herd status emacs
```

### Development Workflow

```bash
# Enter development shell with dependencies
guix shell <package1> <package2> -- bash

# Use direnv for project-specific environments
# .envrc files automatically activate when entering directories

# 3D Slicer profile management
setup-guix-slicer-profile.sh 5  # or 6 for different versions
```

### Window Manager (bspwm)

```bash
# Restart bspwm
bspc quit

# Reload sxhkd (keybindings)
pkill -USR1 sxhkd

# Check polybar
~/.config/polybar.local/launch.sh
```

## System Components

### Desktop Environment Stack
- **Window Manager**: bspwm (binary space partition tiling)
- **Hotkey Daemon**: sxhkd (Super key prefix)
- **Compositor**: picom (transparency, shadows, animations)
- **Status Bar**: polybar (per-monitor themes)
- **Terminal**: Alacritty
- **Editor**: Emacs (lucid build, daemon mode with auto-restart)
- **Display Server**: X11 (xlibre variant for custom libinput/GPU drivers)
- **Color Scheme**: Pywal (dynamic theming from wallpapers)

### Key Services
- **Audio**: PipeWire (replaces PulseAudio/JACK)
- **Virtualization**: libvirt, QEMU, Podman
- **VPN**: Tailscale
- **Email**: isync + msmtp + mutt_oauth2 (OAuth2 for institutional accounts)
- **Security**: GPG agent, SSH, Fail2Ban, AIDE (file integrity)
- **Power Management**: TLP (laptop), thermald

### System-Specific Features

**Einstein (Desktop)**:
- NVIDIA GPU support (nvidia-driver)
- SLiM display manager
- Container tools (Podman, Distrobox)
- AIDE file integrity monitoring
- More development packages

**Curie (Laptop)**:
- AMD GPU (amdgpu + mesa + xlibre)
- Lighter configuration
- Enhanced power management (TLP, thermald)
- Battery monitoring

## Configuration Patterns

### Adding New Packages

1. **System-level package (all machines)**: Edit `entelequia/system/lib/common-packages.scm`, add to appropriate category
2. **System-level package (machine-specific)**: Edit `entelequia/system/machines/einstein.scm` or `curie.scm`, add to packages list
3. **Home-level package (all machines)**: Edit `entelequia/home/profiles/base.scm`, `development.scm`, or `email.scm`
4. **Home-level package (machine-specific)**: Edit `entelequia/home/home-config.scm`, add to packages list
5. **Custom package**: Create in `entelequia/packages/`, follow existing patterns (see `emacs.scm`, `fonts.scm`)

### Creating New Services

1. **Home service**: Add to `entelequia/home/services/` (see `emacs.scm`, `desktop.scm`, `gpg.scm`, `shell.scm`)
2. **System service (shared)**: Add to `entelequia/system/lib/common-services.scm` for reusable services
3. **System service (layer-specific)**: Add to `entelequia/system/layers/base.scm`, `desktop-base.scm`, or `server-base.scm`
4. **System service (machine-specific)**: Add to `entelequia/system/machines/einstein.scm` or `curie.scm`
5. Use `simple-service` for basic configurations (environment variables, udev rules, etc.)

### Adding a New Machine

1. Create new file: `entelequia/system/machines/new-machine.scm`
2. Define `machine-config` with hostname, GPU type, etc.:
   ```scheme
   (define new-machine-config
     (machine-config
      (hostname "new-machine")
      (username "rafael")
      (gpu-type 'intel)  ; or 'nvidia, 'amd
      (machine-type 'laptop)))  ; or 'desktop
   ```
3. Inherit from appropriate base layer (desktop-base or server-base)
4. Add machine-specific file-systems, bootloader, and packages
5. Test with: `./scripts/test-vm.sh` (create VM config first)

### Modifying Emacs Configuration

1. Edit `emacs.org` (literate configuration)
2. Tangle with `C-c C-v t` or on save (if configured)
3. Restart Emacs daemon: `herd restart emacs`
4. Or manually: `emacsclient -e '(kill-emacs)' && emacs --daemon`

### Adding Dotfiles

1. Place files in `dotfiles/` directory structure (mirrors home directory)
2. Run `guix home reconfigure` to symlink into home
3. Files are automatically deployed via `home-dotfiles-service-type`

## Module System

The codebase uses Guile Scheme modules with a layered architecture:

```scheme
(define-module (entelequia system layers base)
  #:use-module (gnu)
  #:use-module (entelequia lib records)
  #:use-module (entelequia system lib common-services)
  #:export (make-base-operating-system))
```

- **Import modules**: `#:use-module (module name)`
- **Export definitions**: `#:export (function-name variable-name)`
- **Use package shortcuts**: `use-package-modules`, `use-service-modules`

### Key Module Patterns

**Layered OS Construction**:
```scheme
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
```

**Using machine-config Record**:
```scheme
(use-modules (entelequia lib records))

(define my-config
  (machine-config
   (hostname "my-machine")
   (username "user")
   (gpu-type 'nvidia)      ; 'nvidia, 'amd, or 'intel
   (machine-type 'desktop) ; 'desktop or 'laptop
   (timezone "Europe/Oslo")
   (locale "en_US.utf8")))
```

## Important Files

### Critical Configuration
- `channels.scm` - Package source definitions (commit-pinned for reproducibility)
- `entelequia/lib/records.scm` - Core `machine-config` record type definition
- `entelequia/lib/helpers.scm` - GPU and package helper functions
- `entelequia/system/layers/base.scm` - Shared OS base (~71 services: kernel, bootloader, networking, virtualization, security)
- `entelequia/system/layers/desktop-base.scm` - Desktop layer (adds desktop packages to base)
- `entelequia/system/lib/common-packages.scm` - Shared package lists (8 categories: hardware, audio, bluetooth, X11, security, etc.)
- `entelequia/system/lib/common-services.scm` - Reusable service definitions (AIDE, bluetooth, libvirt, polkit, etc.)
- `entelequia/system/lib/security-hardening.scm` - Comprehensive security hardening (kernel, firewall, fail2ban, audit, SSH)
- `entelequia/home/home-config.scm` - Base home environment (packages, dotfiles service, bash config)

### Machine Configurations
- `entelequia/system/machines/einstein.scm` - Desktop system (NVIDIA, AIDE, Podman, more packages)
- `entelequia/system/machines/curie.scm` - Laptop system (AMD, TLP, thermald, power management)

### Home Environment
- `entelequia/home/profiles/base.scm` - Essential packages (git, compression, text processing)
- `entelequia/home/profiles/development.scm` - Development tools (GCC, Node.js, Python, debugging)
- `entelequia/home/profiles/email.scm` - Email stack with OAuth2 support
- `entelequia/home/services/emacs.scm` - Emacs daemon service with 60+ packages
- `entelequia/home/services/desktop.scm` - Desktop services (D-Bus, PipeWire, GPG, polybar)
- `entelequia/home/services/gpg.scm` - GPG agent configuration
- `entelequia/home/services/shell.scm` - Shell configuration
- `entelequia/systems/desktop.scm` - Shared desktop home services (imported by both systems)

### Test Infrastructure
- `entelequia/system/vms/test-desktop.scm` - Desktop VM for testing before deployment
- `entelequia/system/vms/test-server.scm` - Server VM for testing
- `entelequia/tests/vm-runner.scm` - VM test harness
- `scripts/test-vm.sh` - Script to launch test VMs
- `scripts/validate-refactor.sh` - Validation script for module syntax
- `scripts/deploy.sh` - Deployment script

### Custom Packages
- `entelequia/packages/emacs.scm` - Custom Emacs packages (denote-silo, evil-snipe, ob-mermaid, persp-projectile)
- `entelequia/packages/fonts.scm` - Nerd fonts (CommitMono, Iosevka, JetBrainsMono, etc.)
- `entelequia/packages/polybar-themes.scm` - Polybar theme collection
- `entelequia/packages/cyrus-sasl-xoauth2.scm`, `mutt-oauth2.scm` - OAuth2 email authentication

### User Dotfiles
- `dotfiles/.xsession` - X11 startup (compositor, WM, notifications)
- `dotfiles/.config/bspwm/bspwmrc` - Window manager config
- `dotfiles/.config/sxhkd/sxhkdrc` - Keybindings (Super key shortcuts)
- `dotfiles/.config/alacritty/alacritty.toml` - Terminal emulator
- `dotfiles/.local/bin/` - User scripts (wallpaper, keyboard layout, aider wrapper)

## Testing Changes

### System Configuration Testing

1. **Validate module syntax** (fast, no builds):
   ```bash
   ./scripts/validate-refactor.sh
   ```

2. **Test system build** (dry-run, no sudo needed):
   ```bash
   guix time-machine -C channels.scm -- system build -L . \
     entelequia/system/machines/einstein.scm --dry-run
   ```

3. **Test in VM** (safest, isolated environment):
   ```bash
   # Desktop VM test
   ./scripts/test-vm.sh test-desktop

   # Server VM test
   ./scripts/test-vm.sh test-server
   ```

4. **Dry-run on actual hardware** (shows changes without applying):
   ```bash
   sudo guix time-machine -C channels.scm -- \
     system reconfigure entelequia/system/machines/einstein.scm --dry-run
   ```

5. **Apply to actual hardware**:
   ```bash
   sudo guix time-machine -C channels.scm -- \
     system reconfigure entelequia/system/machines/einstein.scm
   ```

6. **Rollback if needed**: `sudo guix system roll-back`

### Home Environment Testing

1. **Test home build**: `guix home build entelequia/home/home-config.scm`
2. **Dry-run**: `guix home reconfigure --dry-run entelequia/home/home-config.scm`
3. **Apply changes**: `guix home reconfigure entelequia/home/home-config.scm`
4. **Rollback if needed**: `guix home roll-back`

### Best Practice Testing Workflow

For major changes, follow this sequence:
1. Run `./scripts/validate-refactor.sh` (syntax check)
2. Test in VM with `./scripts/test-vm.sh test-desktop`
3. Dry-run on actual hardware (laptop first, easier to rollback)
4. Deploy to laptop (curie), test for 24-48 hours
5. Deploy to desktop (einstein) after laptop proves stable

## Troubleshooting

### Service Issues
```bash
# Check shepherd (service manager) status
herd status

# Restart specific service
herd restart <service-name>

# View service logs
herd log <service-name>
```

### Build Failures
```bash
# Clear build cache
guix gc

# Update channels
guix pull --channels=channels.scm

# Check for substitute availability
guix weather <package-name>
```

### Emacs Daemon Issues
```bash
# Check daemon status
herd status emacs

# View logs
herd log emacs

# Restart daemon
herd restart emacs

# Kill and restart manually
emacsclient -e '(kill-emacs)'
emacs --daemon
```

## Security Notes

The system implements comprehensive security hardening through the `security-hardening` module:

### Access Control
- **SSH**: Hardened configuration on port 2222
  - Key-only authentication (password auth disabled)
  - Root login disabled
  - Strong ciphers and MACs (ChaCha20-Poly1305, AES-GCM, SHA2)
  - Connection rate limiting (4 connections/minute)
  - Maximum 3 authentication attempts
- **User access**: `rafael` in wheel group for sudo
- **GPG agent**: Rofi for graphical password prompts
- **OAuth2**: Institutional email (NTNU/UiO accounts)

### Network Security
- **Firewall**: nftables with stateful packet filtering
  - Default DROP policy for incoming traffic
  - Allows: SSH (2222), Tailscale (41641), mDNS (5353), established connections
  - Einstein-specific: Synergy port (24800) for keyboard/mouse sharing
  - Rate-limited ICMP (1 packet/second)
  - Connection tracking with reasonable limits
  - Logging of dropped packets
  - Machine-specific ports can be added via `firewall-extra-tcp-ports` and `firewall-extra-udp-ports` parameters
- **Fail2Ban**: SSH brute-force protection
  - Ban after 3 failed attempts
  - 2-hour ban duration, 10-minute detection window
  - Automatic IP blocking
- **Tailscale VPN**: System-wide secure mesh networking

### Kernel Hardening
- **Network**: SYN cookies, IP forwarding disabled, source validation, martian packet logging
- **Process isolation**: Restricted ptrace, disabled unprivileged BPF, restricted user namespaces
- **Memory protection**: ASLR enabled, mmap restrictions, kexec disabled
- **Information hiding**: dmesg restricted, kptr restricted, perf events restricted
- **Filesystem**: Protected hardlinks/symlinks/FIFOs, disabled SUID core dumps

### Filesystem Security
- **/tmp**: Mounted with `noexec`, `nosuid`, `nodev` flags
- **AIDE**: File integrity monitoring (Einstein only)
  - Monitors /bin, /sbin, /usr/bin, /usr/sbin, /etc, /boot, /lib, /gnu/store
  - SHA256 checksums with permission tracking
  - Configuration at `/etc/aide/aide.conf`
  - Manual check: `herd start aide-check`
  - Database: `/var/lib/aide/aide.db`

### Audit & Monitoring
- **Auditd**: Security event logging
  - Authentication events (/etc/passwd, /etc/shadow, /etc/sudoers)
  - SSH configuration changes
  - Kernel module operations
  - System time changes, mounts, file deletions
  - Privileged command execution
  - Logs: `/var/log/audit.log`
- **PAM limits**: Process limits, core dumps disabled
  - Regular users: 1024 soft / 4096 hard process limit
  - Core dumps completely disabled for security

### Security Commands
```bash
# Check firewall rules
sudo nft list ruleset

# Reload firewall
herd reload nftables

# Check Fail2Ban status
herd status fail2ban

# View banned IPs
sudo fail2ban-client status sshd

# Run AIDE file integrity check
herd start aide-check

# View audit logs
sudo ausearch -ts today

# View kernel hardening parameters
sysctl kernel.yama.ptrace_scope
sysctl kernel.unprivileged_bpf_disabled
```

### Security Configuration Files
- Kernel hardening: `/etc/sysctl.d/99-security-hardening.conf`
- Firewall rules: `/etc/nftables.conf`
- Fail2Ban: `/etc/fail2ban/jail.local`
- AIDE config: `/etc/aide/aide.conf`
- Audit rules: `/etc/audit/audit.rules`
- SSH config: Generated from `hardened-ssh-service`

### Important Notes
- Firewall allows all outgoing connections (can be restricted if needed)
- AIDE database must be initialized after first install
- Some security features may need adjustment for specific use cases (e.g., containers need user namespaces)
- Audit logs can grow large; rotate regularly

## Cryptographic Key Infrastructure (GPG)

This system uses a **maximum security GPG architecture** with an offline master key and online subkeys. This approach provides strong security while maintaining daily usability.

### Architecture Overview

**Master Key + Subkeys Model**:
- **Master Key** (`[C]` - Certification only): Kept offline, used only for key management
- **Signing Subkey** (`[S]`): Git commits, documents, package signatures
- **Encryption Subkey** (`[E]`): Email, file encryption
- **Authentication Subkey** (`[A]`): SSH authentication via GPG agent

**Key Benefits**:
- ✅ Revoke/rotate subkeys without changing identity
- ✅ Compromise of daily-use key doesn't compromise master key
- ✅ Master key stays offline on encrypted backup
- ✅ Single key infrastructure for multiple purposes
- ✅ Can have different expiration policies per subkey

### Directory Structure

```
~/.keys/
├── gpg/
│   ├── master/                          # OFFLINE storage (encrypted USB/separate disk)
│   │   ├── master-key.asc.gpg          # Master key encrypted with strong passphrase
│   │   ├── master-key-backup.tar.gpg   # Full GNUPGHOME backup
│   │   ├── revocation-cert.asc         # CRITICAL: print and store securely
│   │   └── README.md                   # Key generation metadata (date, fingerprint)
│   ├── public/                          # Can be version controlled in dotfiles
│   │   ├── public-key.asc              # Public key (all subkeys)
│   │   └── fingerprint.txt             # Key fingerprint for verification
│   └── subkeys-backup/                  # Encrypted subkey backups
│       ├── subkeys.tar.gpg             # Periodic encrypted backups
│       └── restore-instructions.md      # Recovery procedures
├── ssh/
│   ├── id_ed25519_github               # GitHub-specific key (if not using GPG auth)
│   ├── id_ed25519_servers              # Server access keys
│   └── config                          # SSH config (can be in dotfiles)
└── README.md                           # Key management procedures
```

### Initial Master Key Generation

**IMPORTANT**: Perform this on an air-gapped machine or bootable Linux USB for maximum security. For practical security, a clean system with network disabled is acceptable.

#### 1. Prepare Secure Environment

```bash
# Create key directory structure
mkdir -p ~/.keys/gpg/{master,public,subkeys-backup}
mkdir -p ~/.keys/ssh

# Optional: Disconnect network for key generation
nmcli networking off  # or physically disconnect

# Set restrictive permissions
chmod 700 ~/.keys
chmod 700 ~/.keys/gpg/master
```

#### 2. Generate Master Key (Certification Only)

```bash
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
```

**Record Key Information**:
```bash
# Get your key fingerprint
gpg --list-keys --keyid-format LONG your-email@example.com

# Save fingerprint to file
gpg --fingerprint your-email@example.com | tee ~/.keys/gpg/public/fingerprint.txt

# Export master key ID
export KEYID="YOUR_KEY_ID_HERE"  # e.g., 0xABCDEF1234567890
```

#### 3. Generate Subkeys

**Signing Subkey** (for Git commits, documents):
```bash
gpg --expert --edit-key $KEYID
gpg> addkey
# Select: (4) RSA (sign only)
# Key size: 4096 bits
# Expiration: 2y (2 years - shorter than master for rotation)
gpg> save
```

**Encryption Subkey** (for email, files):
```bash
gpg --expert --edit-key $KEYID
gpg> addkey
# Select: (6) RSA (encrypt only)
# Key size: 4096 bits
# Expiration: 2y (2 years)
gpg> save
```

**Authentication Subkey** (for SSH):
```bash
gpg --expert --edit-key $KEYID
gpg> addkey
# Select: (8) RSA (set your own capabilities)
# Toggle off Sign and Encrypt, toggle on Authenticate
# Key size: 4096 bits
# Expiration: 2y (2 years)
gpg> save
```

#### 4. Generate Revocation Certificate

**CRITICAL**: Generate and secure this immediately:

```bash
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
```

#### 5. Backup Master Key

```bash
# Export secret master key (encrypted with your passphrase)
gpg --export-secret-keys --armor $KEYID > \
    ~/.keys/gpg/master/master-key.asc

# Create encrypted backup of entire GNUPGHOME
tar czf - ~/.gnupg | gpg --symmetric --cipher-algo AES256 \
    --output ~/.keys/gpg/master/master-key-backup.tar.gpg

# Verify backup integrity
gpg --decrypt ~/.keys/gpg/master/master-key-backup.tar.gpg | tar tzf - > /dev/null
echo "Backup verified: $?"  # Should be 0
```

#### 6. Export Public Key

```bash
# Export public key (shareable)
gpg --export --armor $KEYID > ~/.keys/gpg/public/public-key.asc

# Optionally, upload to key servers
gpg --send-keys $KEYID

# Or upload to specific keyserver
gpg --keyserver keys.openpgp.org --send-keys $KEYID
```

#### 7. Create Subkey-Only Keyring (Daily Use)

**Critical step**: Remove master key from daily-use keyring for security.

```bash
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
```

### Key Storage Strategy

**Master Key Storage** (choose one or multiple):
1. **Encrypted USB drive**: Keep in safe/lockbox, preferably off-site
2. **Encrypted external disk**: Secondary location (parent's house, bank vault)
3. **Paper backup**: QR code or printed ASCII-armored key (fire-proof safe)
4. **Encrypted cloud backup**: As last resort, triple-encrypted (GPG + VeraCrypt + provider encryption)

**DO NOT**:
- ❌ Store master key on daily-use machine
- ❌ Store in unencrypted cloud storage
- ❌ Keep only one copy (redundancy is critical)
- ❌ Store revocation certificate with master key (separate locations)

**Subkeys** (daily use):
- ✅ Stored in `~/.gnupg` (encrypted by GPG agent)
- ✅ Backed up encrypted to `~/.keys/gpg/subkeys-backup/`
- ✅ Can be revoked/rotated without affecting identity

### Daily Usage Workflow

#### Git Commit Signing

```bash
# Configure Git to use GPG (already in your Guix config)
git config --global user.signingkey $KEYID
git config --global commit.gpgsign true
git config --global tag.gpgSign true

# Sign commits automatically
git commit -m "Your commit message"  # Automatically signed

# Verify signatures
git log --show-signature
```

#### Email Encryption (with your existing setup)

Your system already has GPG configured for email via `entelequia/home/services/gpg.scm`. Ensure your email client is configured:

```bash
# Export public key for correspondents
gpg --armor --export your-email@example.com > my-public-key.asc

# Decrypt email
gpg --decrypt encrypted-message.asc

# Encrypt file for someone
gpg --encrypt --recipient their-email@example.com document.pdf
```

#### SSH Authentication via GPG

**Enable GPG SSH support** (add to `~/.bashrc` or Guix shell config):

```bash
# Add to shell configuration
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# Add to ~/.gnupg/gpg-agent.conf (or Guix GPG service config)
enable-ssh-support
default-cache-ttl 3600
max-cache-ttl 7200
```

**Extract SSH public key from GPG**:

```bash
# Get SSH public key from authentication subkey
gpg --export-ssh-key $KEYID > ~/.keys/ssh/gpg-auth-key.pub

# Add to remote servers
ssh-copy-id -i ~/.keys/ssh/gpg-auth-key.pub user@server

# Test authentication
ssh -v user@server  # Should use GPG key
```

### Key Maintenance and Rotation

#### Extending Subkey Expiration

```bash
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
```

#### Rotating Compromised Subkeys

```bash
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
```

#### Complete Key Revocation (Emergency)

**Only if master key is compromised or permanently lost**:

```bash
# Import revocation certificate
gpg --import ~/.keys/gpg/master/revocation-cert.asc

# Upload to key servers
gpg --send-keys $KEYID

# Notify contacts and generate new key infrastructure
```

### Integration with Guix System

Your GPG configuration is already in `entelequia/home/services/gpg.scm`. Enhance it to include:

```scheme
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
```

**Declarative Git Configuration** (add to `home-config.scm` or dotfiles):

```bash
# In ~/.gitconfig or via Guix home-git-service
[user]
    name = Rafael [Your Name]
    email = your-email@example.com
    signingkey = YOUR_KEY_ID_HERE
[commit]
    gpgsign = true
[tag]
    gpgSign = true
```

### Best Practices

#### Security Practices

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

#### Operational Practices

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
   - Weekly: Encrypted subkey backup to `~/.keys/gpg/subkeys-backup/`
   - Monthly: Verify master key backup is accessible
   - Annually: Test full key restoration from backup

4. **Documentation**:
   - Keep key generation date and parameters in `~/.keys/gpg/master/README.md`
   - Document all subkey rotations with dates
   - Maintain list of where public key is published

### Common GPG Commands

```bash
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
```

### Troubleshooting

#### GPG Agent Not Working

```bash
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
```

#### "No Secret Key" Errors

```bash
# Verify subkeys are present
gpg --list-secret-keys

# If missing, restore from backup
gpg --import ~/.keys/gpg/subkeys-backup/subkeys.tar.gpg

# Check keygrip (links secret keys to GPG agent)
gpg --with-keygrip --list-secret-keys
ls -la ~/.gnupg/private-keys-v1.d/
```

#### SSH Authentication Not Using GPG

```bash
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
```

#### Passphrase Prompt Issues (Rofi)

```bash
# Check pinentry program (should be rofi for graphical prompts)
gpgconf --list-components | grep pinentry

# Update pinentry (in Guix GPG service or ~/.gnupg/gpg-agent.conf)
# pinentry-program /path/to/pinentry-rofi

# Test pinentry
echo "GETPIN" | pinentry
```

### Quick Reference Card

**Key Capabilities**:
- `[C]` = Certification (master key, sign other keys)
- `[S]` = Signing (sign commits, documents, packages)
- `[E]` = Encryption (encrypt emails, files)
- `[A]` = Authentication (SSH, authentication tokens)

**Key Status Indicators**:
- `sec` = Secret key present
- `sec#` = Secret key not present (stub only - GOOD for daily use)
- `ssb` = Secret subkey present
- `pub` = Public key

**Expiration Policy**:
- Master key: No expiration (permanent identity)
- Subkeys: 2-year expiration (rotate regularly)

**Backup Locations**:
1. Master key: Offline encrypted storage (USB, external disk, safe)
2. Revocation cert: Printed paper in safe (separate from master key)
3. Subkeys: Encrypted backup in `~/.keys/gpg/subkeys-backup/`
4. Public key: Version controlled in dotfiles, uploaded to keyservers

**Emergency Contacts**:
- If master key lost: Use revocation certificate, generate new key
- If subkey compromised: Revoke subkey, generate new one with master key
- If all keys lost: Use revocation certificate, start over

## Performance Considerations

- Guix uses `/gnu/store` for immutable packages (can grow large, use `guix gc` regularly)
- Home generations accumulate (clean old ones: `guix home delete-generations 1m`)
- Emacs daemon stays running (restart after package changes)
- PipeWire replaces PulseAudio for lower latency audio
- TLP manages laptop power automatically
- Pywal generates color schemes (may cause brief lag when changing wallpapers)
