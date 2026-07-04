#!/usr/bin/env bash
# Local tarball snapshot of org / notes / PKS — offline safety net
# in addition to Nextcloud-side sync history.
#
# Sources (all follow-symlinks so ~/pks → ~/Nextcloud/PKS is captured
# as the real tree, not as a bare symlink):
#   ~/org/                  legacy org agenda files
#   ~/Notes/                contains Work-legacy (read-only archive)
#   ~/Nextcloud/PKS/        the live PKS tree (notes + library)

BACKUP_DIR="$HOME/backups/org"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

mkdir -p "$BACKUP_DIR"

# -h dereferences top-level symlinks; we want the real PKS content.
tar -czhf "$BACKUP_DIR/org-backup-$TIMESTAMP.tar.gz" \
    -C "$HOME" \
    org Notes Nextcloud/PKS 2>/dev/null

# Keep only last 30 backups
cd "$BACKUP_DIR" && ls -t org-backup-*.tar.gz | tail -n +31 | xargs -r rm

notify-send "Org Backup" "Backup completed" -i document-save
