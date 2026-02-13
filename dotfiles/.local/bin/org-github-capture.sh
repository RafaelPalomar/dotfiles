#!/usr/bin/env bash
# Capture a specific GitHub issue to org-mode

# Step 1: Get repo
REPO=$(gh repo list --limit 100 --json nameWithOwner --jq '.[].nameWithOwner' \
       | rofi -dmenu -p "Repository" -i)
[[ -z "$REPO" ]] && exit 0

# Step 2: Get issue
ISSUE=$(gh issue list --repo "$REPO" --limit 50 --json number,title \
        --jq '.[] | "\(.number): \(.title)"' \
        | rofi -dmenu -p "Issue" -i)
[[ -z "$ISSUE" ]] && exit 0

ISSUE_NUM=$(echo "$ISSUE" | cut -d: -f1)

# Step 3: Fetch issue details
ISSUE_DATA=$(gh issue view "$ISSUE_NUM" --repo "$REPO" --json number,title,body,url,state,labels)

TITLE=$(echo "$ISSUE_DATA" | jq -r '.title')
BODY=$(echo "$ISSUE_DATA" | jq -r '.body // ""')
URL=$(echo "$ISSUE_DATA" | jq -r '.url')
STATE=$(echo "$ISSUE_DATA" | jq -r '.state')
LABELS=$(echo "$ISSUE_DATA" | jq -r '.labels | map(.name) | join(", ")')

# Step 4: Create org entry via emacsclient
emacsclient -e "(progn
  (org-capture nil \"g\")
  (insert \"$TITLE\")
  (org-set-property \"REPO\" \"$REPO\")
  (org-set-property \"ISSUE\" \"$ISSUE_NUM\")
  (org-set-property \"STATE\" \"$STATE\")
  (org-set-property \"GITHUB_URL\" \"$URL\")
  (org-set-property \"LABELS\" \"$LABELS\")
  (forward-line 2)
  (insert \"$BODY\")
  (org-capture-finalize))" &>/dev/null

notify-send "GitHub Capture" "Issue #$ISSUE_NUM captured" -i github
