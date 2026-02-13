#!/usr/bin/env bash
# Sync GitHub issues to org-mode

REPOS_FILE="$HOME/.config/org-github-repos"
ORG_FILE="$HOME/org/github-issues.org"

# Check if repos file exists
if [[ ! -f "$REPOS_FILE" ]]; then
    echo "# Add repositories to sync (format: owner/repo)" > "$REPOS_FILE"
    echo "# Example: RafaelPalomar/dotfiles" >> "$REPOS_FILE"
    exit 1
fi

# Start org file
cat > "$ORG_FILE" << 'EOF'
#+TITLE: GitHub Issues
#+FILETAGS: :github:

* Open Issues
EOF

# Read repos and fetch issues
while IFS= read -r REPO; do
    # Skip comments and empty lines
    [[ "$REPO" =~ ^#.*$ ]] || [[ -z "$REPO" ]] && continue

    echo "Syncing $REPO..." >&2

    # Fetch issues using gh CLI
    gh issue list --repo "$REPO" --limit 100 --json number,title,state,labels,url,body \
        --jq '.[] | "** TODO \(.title)\n:PROPERTIES:\n:REPO: '"$REPO"'\n:ISSUE: \(.number)\n:STATE: \(.state)\n:GITHUB_URL: \(.url)\n:LABELS: \(.labels | map(.name) | join(\", \"))\n:END:\n\n\(.body // \"\")\n"' \
        >> "$ORG_FILE"
done < "$REPOS_FILE"

# Add to agenda files if not present
emacsclient -e "(unless (member \"$ORG_FILE\" org-agenda-files) \
                  (add-to-list 'org-agenda-files \"$ORG_FILE\"))" &>/dev/null

notify-send "GitHub Sync" "Issues synced to org-mode" -i github
