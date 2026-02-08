#!/usr/bin/env bash

# Get current layout
current=$(setxkbmap -query | grep layout | awk '{print $2}')

# Cycle through layouts: us -> no -> es -> us
case "${current}" in
    us)
        setxkbmap -layout no
        ;;
    no)
        setxkbmap -layout es
        ;;
    es)
        setxkbmap -layout us
        ;;
    *)
        setxkbmap -layout us
        ;;
esac
