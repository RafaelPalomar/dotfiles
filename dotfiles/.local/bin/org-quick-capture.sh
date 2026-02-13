#!/usr/bin/env bash
# Quick task capture via Rofi

# Step 1: Get task text
TASK=$(rofi -dmenu -p "Quick Task" -theme-str 'window {width: 50%;}')
[[ -z "$TASK" ]] && exit 0

# Step 2: Choose context
CONTEXT=$(echo -e "work\npersonal" | rofi -dmenu -p "Context")
[[ -z "$CONTEXT" ]] && CONTEXT="work"

# Step 3: Capture via emacsclient
if [[ "$CONTEXT" == "personal" ]]; then
    TEMPLATE="tp"
else
    TEMPLATE="tt"
fi

emacsclient -e "(org-capture nil \"$TEMPLATE\")" \
            -e "(insert \"$TASK\")" \
            -e "(org-capture-finalize)" &>/dev/null

# Step 4: Notify
if [[ $? -eq 0 ]]; then
    notify-send "Org Capture" "Task captured: $TASK" -i checkbox-checked
else
    notify-send "Org Capture" "Failed to capture task" -i dialog-error -u critical
fi
