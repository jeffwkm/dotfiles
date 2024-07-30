#!/usr/bin/env bash
set -euo pipefail

this="$(basename "$0")"
msg() { echo -e "[$this] $*"; }

# usage: priority <nice> <process_names>
priority() {
  local pids
  pids="$(pidof "${@:2}")"
  # msg "setting priority $1\t:: ${@:2}"
  # msg "running : sudo renice  <${@:2} :: $pids>"
  [ -n "$pids" ] && sudo renice "$1" $pids >/dev/null
}

set +u
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
  looking-glass-client -C ~/.looking-glass-client.ini "$@"
  exit 0
# elif [ -n "$(pidof looking-glass-client)" ]; then
#     msg "looking-glass-client process already running"
#     exit 1
else
  kill -9 $(pidof looking-glass-client) || true
  looking-glass-client -C ~/.looking-glass-client.ini "$@" &
  sleep 0.1
  set -u
  if [ -n "$(pidof looking-glass-client)" ]; then
    sleep 1
    priority -10 scream
    priority -10 pulseaudio
    priority -15 looking-glass-client sway
    priority 5 waybar
    priority 5 chrome
    priority 5 chromium
    priority 5 firefox
  fi
  fg
fi
