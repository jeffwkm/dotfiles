#!/usr/bin/env bash
set -eu

cmd="$*"

# disable border animation as workaround for defocus bug on rofi exit
hyprctl keyword "animation border,0" >/dev/null
($cmd || true) && sleep 0.05
hyprctl keyword "animation border,1" >/dev/null
