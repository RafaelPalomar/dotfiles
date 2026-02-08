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

## Performance Considerations

- Guix uses `/gnu/store` for immutable packages (can grow large, use `guix gc` regularly)
- Home generations accumulate (clean old ones: `guix home delete-generations 1m`)
- Emacs daemon stays running (restart after package changes)
- PipeWire replaces PulseAudio for lower latency audio
- TLP manages laptop power automatically
- Pywal generates color schemes (may cause brief lag when changing wallpapers)
