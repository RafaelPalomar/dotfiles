#!/usr/bin/env bash
# Delete YOUR OWN posts from one Mattermost conversation, in bulk.
#
# Why this exists as a human tool and not an agent one: `delete_others_posts'
# is system-admin-only in Mattermost, and `mmctl permissions add' is Enterprise,
# so there is no way to grant a bot the narrow power to tidy one DM.  The only
# key that fits opens every door on the server -- so it stays in a human's
# hand.  Ms. Poppins and Mr. Banks each remove their OWN side with `!wipe';
# this is how you remove yours.
#
# It also does something they cannot: --permanent erases attachments from the
# database and filestore.  The bots have no file-delete endpoint at all.
#
# Runs ON edison (mmctl talks to the server over a unix socket in the
# container).  Dry-run unless you pass --yes.
set -euo pipefail

CHANNEL=""; USERNAME="${USER:-rafael}"; NUMBER=1000; PERMANENT=0; YES=0
BATCH=50

usage() {
  cat <<'USAGE'
usage: mm-wipe-mine.sh --channel <id> [options]

  --channel <id>    channel or DM id (find it in the URL, or with `mmctl post list`)
  --user <name>     whose posts to delete (default: $USER)
  --number <n>      how far back to look, in posts (default: 1000)
  --permanent       also erase attachments from the DB and filestore
  --yes             actually delete; without this it only shows what it would do

Deleting is not undoable. --permanent least of all: Mattermost's own help asks
you to have a database backup before using it, and this script will not pretend
otherwise.
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --channel) CHANNEL="$2"; shift 2 ;;
    --user) USERNAME="$2"; shift 2 ;;
    --number) NUMBER="$2"; shift 2 ;;
    --permanent) PERMANENT=1; shift ;;
    --yes) YES=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done
[ -n "$CHANNEL" ] || { echo "error: --channel is required" >&2; usage >&2; exit 2; }

MM=(podman exec -i mattermost mmctl --local)

UID_=$("${MM[@]}" user search "$USERNAME" --json 2>/dev/null \
        | jq -r 'if type=="array" then .[0].id else .id end')
[ -n "$UID_" ] && [ "$UID_" != "null" ] \
  || { echo "error: no such user: $USERNAME" >&2; exit 1; }
echo "user  : $USERNAME ($UID_)"

# Skip posts already deleted, and skip system messages (joins/leaves): those
# are not yours to remove and deleting them leaves odd gaps in the channel.
mapfile -t IDS < <("${MM[@]}" post list "$CHANNEL" --number "$NUMBER" --json 2>/dev/null \
  | jq -r --arg u "$UID_" '.[] | select(.user_id==$u and .delete_at==0
                                       and (.type // "")=="") | .id')

echo "found : ${#IDS[@]} of your posts in the last $NUMBER"
[ "${#IDS[@]}" -gt 0 ] || { echo "nothing to do"; exit 0; }

MODE="mark as deleted"
DELFLAGS=(--confirm)
if [ "$PERMANENT" -eq 1 ]; then
  MODE="PERMANENTLY delete (including attachments)"
  DELFLAGS=(--permanent --confirm)
fi

if [ "$YES" -eq 0 ]; then
  echo
  echo "DRY RUN — would $MODE ${#IDS[@]} posts."
  echo "Re-run with --yes to do it."
  exit 0
fi

echo "action: $MODE"
done_=0
# Batched because a thousand ids on one command line is a fragile way to
# discover your shell's argument limit.
for ((i = 0; i < ${#IDS[@]}; i += BATCH)); do
  chunk=("${IDS[@]:i:BATCH}")
  "${MM[@]}" post delete "${chunk[@]}" "${DELFLAGS[@]}" >/dev/null
  done_=$(( done_ + ${#chunk[@]} ))
  printf '\rdeleted %d/%d' "$done_" "${#IDS[@]}"
done
printf '\ndone\n'
