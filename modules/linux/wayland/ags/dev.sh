#!/usr/bin/env bash
set -eu

cd ~/.config/ags

fd . -e .ts ~/.config/ags | entr -rs "
  ./build
  ags $*
"
