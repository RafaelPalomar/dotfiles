#!/bin/sh
LOG="$HOME/.local/state/edison-deploy.log"
{
  echo "=== deploy (agent) at $(git -C "$HOME/.dotfiles" rev-parse --short HEAD) $(date -Is) ==="
  cd "$HOME/.dotfiles" || exit 1
  guix time-machine -C channels-lock.scm -- deploy -L . entelequia/deploy/edison.scm
  echo "=== deploy exit=$? $(date -Is) ==="
} >>"$LOG" 2>&1
