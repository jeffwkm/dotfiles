#!/usr/bin/env bash
set -eu

pamixer -d 2 --allow-boost
value=$(pamixer --get-volume)
# --hint string:image-path:audio-speakers
notify-send.sh --expire-time=1000 \
   --hint int:transient:1 \
   --hint int:value:$value \
   --replace-file=$HOME/.volume-popup \
   "Volume" "$value% / 100%"
