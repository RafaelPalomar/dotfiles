#!/usr/bin/env bash
# Enhanced GitHub to org-mode sync with PRs, comments, and conflict detection

set -uo pipefail

# Configuration
REPOS_FILE="$HOME/.config/org-github-repos"
ORG_FILE="$HOME/org/github-issues.org"
TEMP_FILE="$HOME/.cache/github-sync-temp.org"
CONFLICT_LOG="$HOME/.local/state/org-github-conflicts.log"
STATE_DIR="$HOME/.local/state"

# Statistics
STATS_NEW=0
STATS_UPDATED=0
STATS_SKIPPED=0
STATS_CONFLICTS=0

# Ensure directories exist
mkdir -p "$(dirname "$ORG_FILE")" "$STATE_DIR" "$(dirname "$TEMP_FILE")"

# Initialize conflict log if it doesn't exist
[[ ! -f "$CONFLICT_LOG" ]] && touch "$CONFLICT_LOG"

# Check if repos file exists
if [[ ! -f "$REPOS_FILE" ]]; then
    cat > "$REPOS_FILE" << 'EOF'
# Add repositories to sync (format: owner/repo)
# Example: RafaelPalomar/dotfiles
# Lines starting with # are ignored
EOF
    echo "Created $REPOS_FILE - please add repositories to sync" >&2
    exit 1
fi

# Function: Log message with timestamp
log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >&2
}

# Function: Format timestamp for org-mode
format_timestamp() {
    local ts="$1"
    if [[ -n "$ts" && "$ts" != "null" ]]; then
        date -d "$ts" +'%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || echo "$ts"
    else
        date -u +'%Y-%m-%dT%H:%M:%SZ'
    fi
}

# Function: Extract property value from org entry
extract_property() {
    local entry="$1"
    local prop="$2"
    echo "$entry" | grep -oP "^:${prop}: \K.*" | head -1 || echo ""
}

# Function: Parse existing org file into associative arrays
declare -A EXISTING_ENTRIES
declare -A EXISTING_ORG_UPDATED
declare -A EXISTING_GITHUB_UPDATED

parse_existing_org() {
    [[ ! -f "$ORG_FILE" ]] && return 0

    local current_entry=""
    local current_key=""
    local in_entry=false
    local in_properties=false

    while IFS= read -r line; do
        # Detect TODO/DONE entries (issues/PRs)
        if [[ "$line" =~ ^\*\*\*[[:space:]]+(TODO|DONE)[[:space:]]+(.+)$ ]]; then
            # Save previous entry if exists
            if [[ -n "$current_key" ]]; then
                EXISTING_ENTRIES["$current_key"]="$current_entry"
            fi

            # Start new entry
            in_entry=true
            in_properties=false
            current_entry="$line"$'\n'
            current_key=""
        elif [[ "$line" =~ ^:PROPERTIES:$ ]] && $in_entry; then
            in_properties=true
            current_entry+="$line"$'\n'
        elif [[ "$line" =~ ^:END:$ ]] && $in_properties; then
            in_properties=false
            current_entry+="$line"$'\n'

            # Extract key (REPO + ISSUE)
            local repo=$(extract_property "$current_entry" "REPO")
            local issue=$(extract_property "$current_entry" "ISSUE")
            local issue_type=$(extract_property "$current_entry" "ISSUE_TYPE")

            if [[ -n "$repo" && -n "$issue" ]]; then
                current_key="${repo}:${issue}:${issue_type}"

                # Extract timestamps
                local gh_updated=$(extract_property "$current_entry" "GITHUB_UPDATED")
                local org_updated=$(extract_property "$current_entry" "ORG_UPDATED")

                [[ -n "$gh_updated" ]] && EXISTING_GITHUB_UPDATED["$current_key"]="$gh_updated"
                [[ -n "$org_updated" ]] && EXISTING_ORG_UPDATED["$current_key"]="$org_updated"
            fi
        elif $in_entry; then
            current_entry+="$line"$'\n'
        fi
    done < "$ORG_FILE"

    # Save last entry
    if [[ -n "$current_key" ]]; then
        EXISTING_ENTRIES["$current_key"]="$current_entry"
    fi
}

# Function: Fetch comments for an issue/PR
fetch_comments() {
    local repo="$1"
    local number="$2"
    local is_pr="$3"

    local endpoint="repos/${repo}/issues/${number}/comments"
    [[ "$is_pr" == "true" ]] && endpoint="repos/${repo}/pulls/${number}/comments"

    gh api "$endpoint" --jq '.[] | "- [\(.created_at | sub("T"; " ") | sub("Z"; ""))] @\(.user.login): \(.body | gsub("\n"; " "))"' 2>/dev/null || echo ""
}

# Function: Format org entry for issue or PR
format_org_entry() {
    local repo="$1"
    local json_data="$2"
    local is_pr="$3"

    local number=$(echo "$json_data" | jq -r '.number')
    local title=$(echo "$json_data" | jq -r '.title')
    local state=$(echo "$json_data" | jq -r '.state' | tr '[:upper:]' '[:lower:]')
    local url=$(echo "$json_data" | jq -r '.url')
    local body=$(echo "$json_data" | jq -r '.body // ""')
    local labels=$(echo "$json_data" | jq -r '.labels | map(.name) | join(", ")')
    local assignees=$(echo "$json_data" | jq -r '.assignees | map(.login) | join(", ")')
    local milestone=$(echo "$json_data" | jq -r '.milestone.title // ""')
    local updated_at=$(echo "$json_data" | jq -r '.updatedAt')
    local comments_data=$(echo "$json_data" | jq -r '.comments // []')
    local comments_count=$(echo "$comments_data" | jq -r 'length')

    # PR-specific fields
    local merged="false"
    local issue_type="issue"
    if [[ "$is_pr" == "true" ]]; then
        merged=$(echo "$json_data" | jq -r '.merged')
        issue_type="pr"
    fi

    # Determine TODO keyword based on state
    local todo_keyword="TODO"
    [[ "$state" == "closed" || "$merged" == "true" ]] && todo_keyword="DONE"

    # Format timestamps
    local github_updated=$(format_timestamp "$updated_at")
    local org_updated=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

    # Build org entry
    cat << EOF
*** $todo_keyword $title
:PROPERTIES:
:REPO: $repo
:ISSUE: $number
:ISSUE_TYPE: $issue_type
:STATE: $state
:GITHUB_URL: $url
:LABELS: $labels
:ASSIGNEES: $assignees
:MILESTONE: $milestone
:GITHUB_UPDATED: $github_updated
:ORG_UPDATED: $org_updated
:COMMENTS_COUNT: $comments_count
EOF

    # Add MERGED property for PRs
    if [[ "$is_pr" == "true" ]]; then
        echo ":MERGED: $merged"
    fi

    echo ":END:"
    echo ""
    echo "$body"

    # Add comments section if comments exist
    if [[ "$comments_count" -gt 0 ]]; then
        echo ""
        echo "** Comments ($comments_count)"
        echo "Last synced: $(date +'%Y-%m-%d %H:%M')"
        echo ""

        # Fetch actual comments
        local comments=$(fetch_comments "$repo" "$number" "$is_pr")
        if [[ -n "$comments" ]]; then
            echo "$comments"
        fi
    fi

    echo ""
}

# Function: Check if entry needs update (timestamp comparison)
should_update_entry() {
    local key="$1"
    local new_github_updated="$2"

    # If entry doesn't exist, always add it
    [[ ! -v EXISTING_ENTRIES["$key"] ]] && return 0

    local existing_gh_updated="${EXISTING_GITHUB_UPDATED[$key]:-}"
    local existing_org_updated="${EXISTING_ORG_UPDATED[$key]:-}"

    # No timestamps? Update to add them
    [[ -z "$existing_gh_updated" || -z "$existing_org_updated" ]] && return 0

    # Convert to seconds for comparison
    local new_gh_ts=$(date -d "$new_github_updated" +%s 2>/dev/null || echo 0)
    local existing_gh_ts=$(date -d "$existing_gh_updated" +%s 2>/dev/null || echo 0)
    local existing_org_ts=$(date -d "$existing_org_updated" +%s 2>/dev/null || echo 0)

    # Check for conflicts: GitHub updated AND local org modified after last sync
    if [[ $new_gh_ts -gt $existing_gh_ts ]] && [[ $existing_org_ts -gt $existing_gh_ts ]]; then
        # Conflict detected
        log "CONFLICT: $key - Both GitHub and local org modified"
        echo "[$(date +'%Y-%m-%d %H:%M:%S')] CONFLICT: $key" >> "$CONFLICT_LOG"
        ((STATS_CONFLICTS++))
        return 1  # Don't update, keep local changes
    fi

    # Update if GitHub is newer
    if [[ $new_gh_ts -gt $existing_gh_ts ]]; then
        return 0  # Update needed
    fi

    return 1  # No update needed
}

# Function: Sync repository issues and PRs
sync_repository() {
    local repo="$1"
    local repo_file="$TEMP_FILE.repo.$(echo "$repo" | tr '/' '-')"

    log "Syncing $repo..."

    # Initialize repo sections
    echo "** Open Issues" > "$repo_file.open-issues"
    echo "** Open PRs" > "$repo_file.open-prs"
    echo "** Closed" > "$repo_file.closed"

    # Fetch issues
    local issues=$(gh issue list --repo "$repo" --limit 100 \
        --json number,title,state,labels,url,body,assignees,milestone,updatedAt,comments 2>/dev/null)

    # Fetch PRs
    local prs=$(gh pr list --repo "$repo" --limit 50 --state all \
        --json number,title,state,labels,url,body,assignees,milestone,updatedAt,comments,merged 2>/dev/null)

    # Process issues
    if [[ -n "$issues" && "$issues" != "[]" ]]; then
        while read -r issue; do
            local number=$(echo "$issue" | jq -r '.number')
            local state=$(echo "$issue" | jq -r '.state' | tr '[:upper:]' '[:lower:]')
            local updated_at=$(echo "$issue" | jq -r '.updatedAt')
            local key="${repo}:${number}:issue"
            local formatted_updated=$(format_timestamp "$updated_at")

            if should_update_entry "$key" "$formatted_updated"; then
                if [[ "$state" == "open" ]]; then
                    format_org_entry "$repo" "$issue" "false" >> "$repo_file.open-issues"
                else
                    format_org_entry "$repo" "$issue" "false" >> "$repo_file.closed"
                fi

                if [[ -v EXISTING_ENTRIES["$key"] ]]; then
                    ((STATS_UPDATED++))
                else
                    ((STATS_NEW++))
                fi
            else
                # Keep existing entry if no update needed
                if [[ -v EXISTING_ENTRIES["$key"] ]]; then
                    echo "${EXISTING_ENTRIES[$key]}" >> \
                        "$([[ "$state" == "open" ]] && echo "$repo_file.open-issues" || echo "$repo_file.closed")"
                fi
                ((STATS_SKIPPED++))
            fi
        done < <(echo "$issues" | jq -c '.[]')
    fi

    # Process PRs
    if [[ -n "$prs" && "$prs" != "[]" ]]; then
        echo "$prs" | jq -c '.[]' | while read -r pr; do
            local number=$(echo "$pr" | jq -r '.number')
            local state=$(echo "$pr" | jq -r '.state' | tr '[:upper:]' '[:lower:]')
            local merged=$(echo "$pr" | jq -r '.merged')
            local updated_at=$(echo "$pr" | jq -r '.updatedAt')
            local key="${repo}:${number}:pr"
            local formatted_updated=$(format_timestamp "$updated_at")

            if should_update_entry "$key" "$formatted_updated"; then
                if [[ "$state" == "open" && "$merged" == "false" ]]; then
                    format_org_entry "$repo" "$pr" "true" >> "$repo_file.open-prs"
                else
                    format_org_entry "$repo" "$pr" "true" >> "$repo_file.closed"
                fi

                if [[ -v EXISTING_ENTRIES["$key"] ]]; then
                    ((STATS_UPDATED++))
                else
                    ((STATS_NEW++))
                fi
            else
                # Keep existing entry if no update needed
                if [[ -v EXISTING_ENTRIES["$key"] ]]; then
                    echo "${EXISTING_ENTRIES[$key]}" >> \
                        "$([[ "$state" == "open" && "$merged" == "false" ]] && echo "$repo_file.open-prs" || echo "$repo_file.closed")"
                fi
                ((STATS_SKIPPED++))
            fi
        done < <(echo "$prs" | jq -c '.[]')
    fi
}

# Function: Build final org file with structure
build_org_file() {
    log "Building org file structure..."

    # Start with header
    cat > "$TEMP_FILE" << 'EOF'
#+TITLE: GitHub Issues & PRs
#+FILETAGS: :github:

EOF

    # Read repos and organize by repository
    while IFS= read -r repo; do
        # Skip comments and empty lines
        [[ "$repo" =~ ^#.*$ ]] || [[ -z "$repo" ]] && continue

        local repo_file="$TEMP_FILE.repo.$(echo "$repo" | tr '/' '-')"

        echo "* $repo" >> "$TEMP_FILE"

        # Append sections if they exist
        if [[ -f "$repo_file.open-issues" ]]; then
            cat "$repo_file.open-issues" >> "$TEMP_FILE"
        fi

        if [[ -f "$repo_file.open-prs" ]]; then
            cat "$repo_file.open-prs" >> "$TEMP_FILE"
        fi

        if [[ -f "$repo_file.closed" ]]; then
            cat "$repo_file.closed" >> "$TEMP_FILE"
        fi

        echo "" >> "$TEMP_FILE"

        # Cleanup repo temp files
        rm -f "$repo_file."*
    done < "$REPOS_FILE"

    # Move temp file to final location
    mv "$TEMP_FILE" "$ORG_FILE"
}

# Main execution
main() {
    log "Starting GitHub sync..."

    # Parse existing org file to detect conflicts
    parse_existing_org

    # Sync each repository
    while IFS= read -r repo; do
        [[ "$repo" =~ ^#.*$ ]] || [[ -z "$repo" ]] && continue
        sync_repository "$repo"
    done < "$REPOS_FILE"

    # Build final org file
    build_org_file

    # Add to org-agenda-files if not present
    if command -v emacsclient &>/dev/null; then
        emacsclient -e "(unless (member \"$ORG_FILE\" org-agenda-files) \
                          (add-to-list 'org-agenda-files \"$ORG_FILE\"))" &>/dev/null || true
    fi

    # Summary statistics
    log "Sync complete: New=$STATS_NEW, Updated=$STATS_UPDATED, Skipped=$STATS_SKIPPED, Conflicts=$STATS_CONFLICTS"

    # Desktop notification
    if command -v notify-send &>/dev/null; then
        local summary="GitHub Sync Complete"
        local body="New: $STATS_NEW | Updated: $STATS_UPDATED | Conflicts: $STATS_CONFLICTS"
        notify-send "$summary" "$body" -i github -t 5000
    fi
}

# Run main function
main "$@"
