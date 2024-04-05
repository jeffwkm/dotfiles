#!/usr/bin/env bash
set -eu

cd ~/.config/ags

bun install

./build-css -w &

fd . -e .ts src/ | entr -nrs "
  bun install
  bunx vite build
  ags $*
"
