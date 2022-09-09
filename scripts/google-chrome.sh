#!/usr/bin/env bash

opts=""
opts+="--force-dark-mode "
opts+="--ignore-gpu-blocklist "
opts+="--enable-zero-copy "
opts+="--enable-gpu-rasterization "
opts+="--enable-pixel-canvas-recording "
opts+="--disable-partial-raster "
# opts+="--disable-zero-copy "
# opts+="--disable-zero-copy-dxgi-video "
# opts+="--in-process-gpu "
# opts+="--disable-skia-renderer "
# opts+="--enable-skia-renderer "
# opts+="--enable-hardware-overlays "
# opts+="--disable-hardware-overlays "
# opts+="--use-vulkan "
# opts+="--use-gl=egl "
# opts+="--use-gl=desktop "
# opts+="--use-gl=swiftshader "
# opts+="--chrome-labs "

unset GDK_SCALE
unset GDK_DPI_SCALE
exec google-chrome-stable "${opts[@]}" "$@" 2>&1
