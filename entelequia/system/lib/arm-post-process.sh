#!/bin/bash
# ARM post-processor — called after each completed rip job.
# ARM invokes: bash /etc/arm/config/post-process.sh "title" "body"
#
# Organises completed video rips into Jellyfin-ready locations:
#   Movies → /home/arm/movies/<Title (Year)>/<Title (Year)>.mkv
#   TV     → /home/arm/tv/<Show>/Season NN/<Show> - SNNENN.mkv
#
# TV episode numbering: counts existing episodes in the season dir and
# continues from there, so disc 2 automatically follows disc 1.
#
# The largest MKV is skipped when it is >1.4× the second-largest —
# that file is the full-disc combined title (e.g. title_t00 on Blu-ray
# that plays all episodes back-to-back).

set -uo pipefail

LOG="/home/arm/logs/post-process.log"
DB="/etc/arm/config/arm.db"
MOVIES_DIR="/home/arm/movies"
TV_DIR="/home/arm/tv"

log() { echo "$(date '+%Y-%m-%d %H:%M:%S') [post-process] $*" >> "$LOG"; }

log "triggered: ${1:-}"

# Query ARM DB for the most recently succeeded video job that has a path.
job_info=$(python3 - <<'PYEOF'
import sqlite3, sys
try:
    conn = sqlite3.connect('/etc/arm/config/arm.db')
    row = conn.execute("""
        SELECT title, video_type, path FROM job
        WHERE status = 'success' AND path IS NOT NULL AND path != ''
        ORDER BY job_id DESC LIMIT 1
    """).fetchone()
    if row:
        print('\t'.join(str(x or '') for x in row))
except Exception as e:
    sys.stderr.write(str(e) + '\n')
PYEOF
)

if [[ -z "$job_info" ]]; then
    log "no completed job found in DB"
    exit 0
fi

IFS=$'\t' read -r title video_type src_dir <<< "$job_info"
log "job: title='$title' type='$video_type' src='$src_dir'"

if [[ ! -d "$src_dir" ]]; then
    log "source dir not found: $src_dir"
    exit 0
fi

# Collect all title MKVs sorted by filename (title_t00, title_t01, …)
mapfile -t all_files < <(find "$src_dir" -maxdepth 1 -name "title_t*.mkv" | sort)
if [[ ${#all_files[@]} -eq 0 ]]; then
    log "no MKV files in $src_dir"
    exit 0
fi

# Remove the combined full-disc title when it is >1.4× the second-largest file.
if [[ ${#all_files[@]} -gt 1 ]]; then
    largest="" largest_sz=0 second_sz=0
    for f in "${all_files[@]}"; do
        sz=$(stat -c%s "$f")
        if [[ $sz -gt $largest_sz ]]; then second_sz=$largest_sz; largest_sz=$sz; largest=$f
        elif [[ $sz -gt $second_sz ]]; then second_sz=$sz; fi
    done
    if [[ $largest_sz -gt $((second_sz * 14 / 10)) ]]; then
        log "removing combined-disc title: $(basename "$largest") (${largest_sz} vs ${second_sz} bytes)"
        rm -f "$largest"
    fi
fi

# Rebuild file list after potential removal
mapfile -t files < <(find "$src_dir" -maxdepth 1 -name "title_t*.mkv" | sort)
if [[ ${#files[@]} -eq 0 ]]; then
    log "no files remaining after filtering"
    exit 0
fi

# Detect TV vs movie from the directory name.
# TV pattern: "Show Name - Season N - Disc M (Year)"
dirname=$(basename "$src_dir")

if echo "$dirname" | grep -qP ' - Season \d+'; then
    # ── TV Show ──────────────────────────────────────────────────────────
    show=$(echo "$dirname" | sed 's/ - Season.*//')
    season=$(echo "$dirname" | grep -oP 'Season \K[0-9]+')
    season_pad=$(printf "%02d" "$season")
    dest="$TV_DIR/$show/Season $season_pad"
    mkdir -p "$dest"

    # Continue episode numbering from however many episodes already exist.
    existing=$(find "$dest" -maxdepth 1 -name "*.mkv" 2>/dev/null | wc -l)
    ep_num=$((existing + 1))

    for f in "${files[@]}"; do
        ep_pad=$(printf "%02d" "$ep_num")
        dst="$dest/$show - S${season_pad}E${ep_pad}.mkv"
        mv "$f" "$dst"
        log "  -> $(basename "$dst")"
        ep_num=$((ep_num + 1))
    done

else
    # ── Movie ─────────────────────────────────────────────────────────────
    # dirname is already "Movie Title (Year)" from ARM/TMDB.
    dest="$MOVIES_DIR/$dirname"
    mkdir -p "$dest"

    # Use the largest file as the main feature; delete any extras.
    main=$(for f in "${files[@]}"; do printf '%s %s\n' "$(stat -c%s "$f")" "$f"; done \
           | sort -rn | head -1 | cut -d' ' -f2-)
    mv "$main" "$dest/$dirname.mkv"
    log "  -> $dirname.mkv"
    rm -f "$src_dir"/title_t*.mkv
fi

# Remove the (now-empty) source directory.
rmdir "$src_dir" 2>/dev/null && log "removed source dir" || true
log "done"
