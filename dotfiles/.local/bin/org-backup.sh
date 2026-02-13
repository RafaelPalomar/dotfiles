#!/usr/bin/env bash
# Backup org and notes directories

BACKUP_DIR="$HOME/backups/org"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

mkdir -p "$BACKUP_DIR"

tar -czf "$BACKUP_DIR/org-backup-$TIMESTAMP.tar.gz" \
    ~/org/ ~/Notes/ 2>/dev/null

# Keep only last 30 backups
cd "$BACKUP_DIR" && ls -t org-backup-*.tar.gz | tail -n +31 | xargs -r rm

notify-send "Org Backup" "Backup completed" -i document-save
