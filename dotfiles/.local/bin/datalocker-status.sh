#!/usr/bin/env bash
#
# DataLocker Status for Polybar
# Returns color code based on device status (icon shown by polybar format-prefix)

# Check if DataLocker is connected (vendor 230a, model 1550)
if lsusb | grep -q "230a:1550"; then
    # Device is connected - check if unlocked (data partition mounted)
    # Check for PRIVATE_USB mount point (the decrypted data partition)
    if mount | grep -q "PRIVATE_USB"; then
        # Unlocked and mounted - green
        echo "%{F#a3be8c}UNLOCKED%{F-}"
    else
        # Connected but locked - yellow
        echo "%{F#ebcb8b}LOCKED%{F-}"
    fi
else
    # Device not connected - return nothing (will show dimmed via polybar)
    echo ""
fi
