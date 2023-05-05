#!/usr/bin/env bash
#amixer sset 'Master' 3%+
pamixer -t
value=$(pamixer --get-volume-human)
notify-send.sh --expire-time=1000 \
  --hint int:transient:1 \
  --hint string:image-path:audio-speakers \
  --replace-file=$HOME/.volume-popup \
  "Volume" "$value / 100%"
