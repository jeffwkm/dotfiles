#!/usr/bin/env bash

source ~/.config/nixpkgs/dotfiles/chromium_dev_keys.sh

name=chromium

opts=(
  ## Needed for Chromium to use Wayland renderer
  "--enable-features=UseOzonePlatform"
  "--ozone-platform=wayland"
  # "--force-device-scale-factor=1.6875"
  ## Try to enable full GPU acceleration
  "--ignore-gpu-blocklist"
  "--enable-zero-copy"
  "--enable-gpu-rasterization"
  # "--enable-direct-composition"
  # "--disable-direct-composition"
  ## These might avoid crash from OOM bug with recent kernels (~ 5.12.x)
  # "--disable-zero-copy-dxgi-video"
  # "--disable-gpu-memory-buffer-video-frames"
  # "--disable-video-capture-use-gpu-memory-buffer"
  # "--disable-gpu-memory-buffer-compositor-resources"
  ## More GPU settings
  "--disable-partial-raster"
  # "--enable-pixel-canvas-recording"
  # "--use-gl=egl"
  # "--enable-accelerated-video-decode"
  # "--disable-accelerated-video-decode"
  # "--in-process-gpu"
  # "--enable-skia-renderer"
  # "--disable-skia-renderer"
  # "--enable-hardware-overlays"
  # "--disable-hardware-overlays"
  # "--disable-sgi-video-sync"
  # "--disable-direct-composition-video-overlays"
  # "--enable-direct-composition-video-overlays"
  # "--disable-running-as-system-compositor"
  # "--enable-native-gpu-memory-buffers"
  # "--use-gl=desktop"
  # "--use-gl=swiftshader"
  # "--enable-features=UseOzonePlatform,Vulkan"
  # "--use-vulkan"
  # "--disable-gpu-sandbox"
  # "--gpu-sandbox-start-early"
  # "--disable-gpu"
  # "--disable-gpu-compositing"
  # "--disable-zero-copy"
  # "--disable-threaded-compositing "
  ## Misc.
  "--force-dark-mode"
  # "--log-level=INFO"
  # "--no-sandbox"
  # "--enable-features=WebUIDarkMode"
  # "--chrome-labs"
);

exec $name "${opts[@]}" "$@" 2>&1
