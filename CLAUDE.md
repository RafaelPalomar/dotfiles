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
├── .files/                   # User dotfiles (deployed via guix-home)
│   ├── .xsession
│   ├── .config/              # Application configs (bspwm, sxhkd, alacritty, etc.)
│   └── .local/bin/           # User scripts
└── entelequia/               # Main Guix module namespace
    ├── home/                 # Base home environment configuration
    ├── home-services/        # Reusable home services (emacs, desktop)
    ├── packages/             # Custom package definitions
    └── systems/              # Complete system configurations
        ├── base.scm          # Shared OS base (kernel, bootloader, services)
        ├── desktop.scm       # Shared desktop home services
        ├── einstein.scm      # Desktop system (NVIDIA GPU)
        └── curie.scm         # Laptop system (AMD GPU)
```

### Key Concepts

- **Declarative System**: Everything from kernel to dotfiles is declared in Scheme
- **Multi-System Support**: Two distinct systems (`einstein` desktop, `curie` laptop) share a common base
- **Guix Channels**: 6 pinned channels provide reproducible package sources (guix, nonguix, guix-xlibre, tailscale, guix-systole, systole-artwork)
- **Home Environment**: Uses Guix Home (not GNU Stow) to manage user packages and dotfiles
- **Literate Configuration**: Emacs config written in org-mode for documentation alongside code

## Common Commands

### System Configuration

```bash
# Apply system configuration (run from ~/.dotfiles)
sudo guix time-machine -C ~/.dotfiles/channels.scm -- system reconfigure entelequia/systems/einstein.scm  # Desktop
sudo guix time-machine -C ~/.dotfiles/channels.scm -- system reconfigure entelequia/systems/curie.scm     # Laptop

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

1. **System-level package**: Edit `entelequia/systems/einstein.scm` or `curie.scm`, add to packages list
2. **Home-level package**: Edit `entelequia/home/home-config.scm`, add to packages list
3. **Custom package**: Create in `entelequia/packages/`, follow existing patterns (see `emacs.scm`, `fonts.scm`)

### Creating New Services

1. **Home service**: Add to `entelequia/home-services/` (see `emacs.scm`, `desktop.scm`)
2. **System service**: Add to services list in `base.scm` or system-specific config
3. Use `simple-service` for basic configurations (environment variables, udev rules, etc.)

### Modifying Emacs Configuration

1. Edit `emacs.org` (literate configuration)
2. Tangle with `C-c C-v t` or on save (if configured)
3. Restart Emacs daemon: `herd restart emacs`
4. Or manually: `emacsclient -e '(kill-emacs)' && emacs --daemon`

### Adding Dotfiles

1. Place files in `.files/` directory structure (mirrors home directory)
2. Run `guix home reconfigure` to symlink into home
3. Files are automatically deployed via `home-dotfiles-service-type`

## Module System

The codebase uses Guile Scheme modules:

```scheme
(define-module (entelequia systems base)
  #:use-module (gnu)
  #:export (base-operating-system guix-home-config))
```

- **Import modules**: `#:use-module (module name)`
- **Export definitions**: `#:export (function-name variable-name)`
- **Use package shortcuts**: `use-package-modules`, `use-service-modules`

## Important Files

### Critical Configuration
- `channels.scm` - Package source definitions (commit-pinned for reproducibility)
- `entelequia/systems/base.scm` - Shared OS base (kernel, bootloader, networking, virtualization)
- `entelequia/home/home-config.scm` - Base home environment (packages, dotfiles service, bash config)
- `entelequia/home-services/emacs.scm` - Emacs daemon service with 60+ packages
- `entelequia/home-services/desktop.scm` - Desktop services (D-Bus, PipeWire, GPG, polybar)

### System Definitions
- `entelequia/systems/einstein.scm` - Desktop system (NVIDIA, more packages)
- `entelequia/systems/curie.scm` - Laptop system (AMD, power management)
- `entelequia/systems/desktop.scm` - Shared desktop home services (imported by both systems)

### Custom Packages
- `entelequia/packages/emacs.scm` - Custom Emacs packages (denote-silo, evil-snipe, ob-mermaid, persp-projectile)
- `entelequia/packages/fonts.scm` - Nerd fonts (CommitMono, Iosevka, JetBrainsMono, etc.)
- `entelequia/packages/polybar-themes.scm` - Polybar theme collection
- `entelequia/packages/cyrus-sasl-xoauth2.scm`, `mutt-oauth2.scm` - OAuth2 email authentication

### User Dotfiles
- `.files/.xsession` - X11 startup (compositor, WM, notifications)
- `.files/.config/bspwm/bspwmrc` - Window manager config
- `.files/.config/sxhkd/sxhkdrc` - Keybindings (Super key shortcuts)
- `.files/.config/alacritty/alacritty.toml` - Terminal emulator
- `.files/.local/bin/` - User scripts (wallpaper, keyboard layout, aider wrapper)

## Testing Changes

1. **Test configuration syntax**: `guix home build entelequia/systems/einstein.scm`
2. **Dry-run**: `guix home reconfigure --dry-run entelequia/systems/einstein.scm`
3. **Apply changes**: `guix home reconfigure entelequia/systems/einstein.scm`
4. **Rollback if needed**: `guix home roll-back`

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

- SSH runs on port 2222 (non-standard)
- User `rafael` has sudo access (wheel group)
- GPG agent uses rofi for pinentry (graphical password prompts)
- OAuth2 configured for institutional email (NTNU/UiO accounts)
- File integrity monitoring via AIDE (Einstein only)
- Fail2Ban enabled for SSH protection
- Tailscale VPN configured system-wide

## Performance Considerations

- Guix uses `/gnu/store` for immutable packages (can grow large, use `guix gc` regularly)
- Home generations accumulate (clean old ones: `guix home delete-generations 1m`)
- Emacs daemon stays running (restart after package changes)
- PipeWire replaces PulseAudio for lower latency audio
- TLP manages laptop power automatically
- Pywal generates color schemes (may cause brief lag when changing wallpapers)
