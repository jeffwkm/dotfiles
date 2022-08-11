#!/usr/bin/env bash

delay() {
  sleep 0.2
}

ui() {
  "$@"
  delay
}

move_ws() {
  set -eu
  local ws_name="$1"
  ui swaymsg -- "workspace $ws_name"
}

ws_primary() {
  set -eu
  sway-move-output-primary
}

layout() {
  set -eu
  local type="$1"
  (ui swaymsg -- "layout $type" >/dev/null) || true
}

layout_stacking() {
  layout "stacking"
}

layout_tabbed() {
  layout "tabbed"
}

layout_split() {
  layout "toggle split"
}

default() {
  layout default
}

splith() {
  ui swaymsg -- "split horizontal"
}

splitv() {
  ui swaymsg -- "split vertical"
}

new_term() {
  set +u
  local dir config config_path
  dir="$1"
  config="$ALACRITTY_CONF"
  if [ -z "$config" ]; then config="alacritty.yml"; fi
  config_path="$HOME/.config/alacritty/$config"
  set -eu
  alacritty --config-file="$config_path" >/dev/null &
  sleep 0.4
  if [ -n "$dir" ]; then
    wtype "cd $dir ; clear"
    wtype -k Return
  fi
  sleep 0.2
}

two_terms() {
  set +u
  local ws_name d1 d2
  ws_name="$1"
  d1="$2"
  d2="$3"
  set -eu
  move_ws "$ws_name"
  default
  new_term "$d1"
  new_term "$d2"
}

three_terms() {
  set +u
  local ws_name d1 d2 d3
  ws_name="$1"
  d1="$2"
  d2="$3"
  d3="$4"
  set -eu
  move_ws "$ws_name"
  default
  new_term "$d1"
  new_term "$d2"
  splitv
  new_term "$d3"
}

four_terms() {
  set +u
  local ws_name d1 d2 d3 d4
  ws_name="$1"
  d1="$2"
  d2="$3"
  d3="$4"
  d4="$5"
  set -eu
  three_terms "$ws_name" "$d1" "$d2" "$d3"
  ui swaymsg -- "focus left"
  splitv
  new_term "$d4"
}

swap_ws_output() {
  ui swaymsg -- "move workspace to output right"
}

start_program_at() {
  set -eu
  local ws_name command delay
  ws_name="$1"
  command="$2"
  delay="$3"
  move_ws "$ws_name"
  bash -c "exec $command" &
  sleep "$delay"
  true
}

# icon_path=/etc/profiles/per-user/jeff/share/icons/Arc/status/128

notify_sway() {
  notify-send.sh --app-name=sway "sway" "$*"
}

notify_action() {
  name="$1"
  cmd="$2"
  prepare="${cmd}__prepare"
  if [ -n "$prepare" ] && command -v "$prepare" >/dev/null; then
    $prepare
  fi
  icon=""
  $cmd || icon=""

  notify_sway "<span weight='bold' size='25pt'>$icon</span>  <span size='18pt'>$name</span>"
}
