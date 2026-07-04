#!/usr/bin/env bash
# Initial Bitwarden setup and login

set -e

LOG_FILE="/tmp/bitwarden-setup.log"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG_FILE"
}

# Check if rbw is installed
if ! command -v rbw &> /dev/null; then
    notify-send -u critical "Bitwarden Setup" "rbw not installed"
    log "ERROR: rbw not installed"
    exit 1
fi

# Check if rofi-rbw is installed
if ! command -v rofi-rbw &> /dev/null; then
    notify-send -u critical "Bitwarden Setup" "rofi-rbw not installed"
    log "ERROR: rofi-rbw not installed. Run: pip install --user rofi-rbw"
    exit 1
fi

# Configure rbw if not already configured
if [ ! -f ~/.config/rbw/config.json ]; then
    log "No existing rbw configuration found"
    EMAIL=$(rofi -dmenu -p "Bitwarden Email" -theme-str 'window {width: 400px;} listview {enabled: false;}')

    if [ -z "$EMAIL" ]; then
        notify-send -u critical "Bitwarden Setup" "Email required"
        log "ERROR: No email provided"
        exit 1
    fi

    rbw config set email "$EMAIL"
    rbw config set lock_timeout 3600
    rbw config set sync_interval 3600
    rbw config set pinentry pinentry-rofi

    log "Configuration saved with email: $EMAIL"
    notify-send "Bitwarden Setup" "Configuration saved"
fi

# Attempt login
log "Attempting Bitwarden login"
if rbw login; then
    log "Login successful"
    notify-send "Bitwarden Setup" "Login successful! Syncing vault..."

    # Initial sync
    if rbw sync; then
        log "Initial sync complete"
        notify-send "Bitwarden Setup" "Setup complete! Use Super+p to open"
    else
        log "ERROR: Sync failed"
        notify-send -u critical "Bitwarden Setup" "Sync failed"
        exit 1
    fi
else
    log "ERROR: Login failed"
    notify-send -u critical "Bitwarden Setup" "Login failed"
    exit 1
fi
