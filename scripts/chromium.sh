#!/usr/bin/env bash

opts=(
  "--force-dark-mode"
  "--enable-features=UseOzonePlatform"
  "--ozone-platform=wayland"
  "--ignore-gpu-blocklist"
  "--enable-zero-copy"
  "--enable-gpu-rasterization"
  "--disable-partial-raster"
  # "--force-device-scale-factor=1.6875"
  # "--enable-pixel-canvas-recording"
  # "--enable-direct-composition"
  # "--disable-direct-composition"
  # "--in-process-gpu"
  # "--enable-skia-renderer"
  # "--disable-skia-renderer"
  # "--enable-hardware-overlays"
  # "--disable-hardware-overlays"
  # "--disable-gpu"
  # "--disable-gpu-compositing"
  # "--log-level=INFO"
  # "--no-sandbox"
  # "--chrome-labs"
);

source "$HOME/.config/nixpkgs/dotfiles/chromium_dev_keys.sh"
exec chromium "${opts[@]}" "$@" 2>&1
