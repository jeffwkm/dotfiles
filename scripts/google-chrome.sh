#!/usr/bin/env bash

#name=chromium
#name=chromium-snapshot-bin

name=google-chrome-stable
#name=google-chrome-beta

if [ ! -z "$1" ] ; then
  name="$1"
fi

#export GDK_BACKEND=wayland
#unset GDK_BACKEND

#unset DISPLAY
unset GDK_SCALE
unset GDK_DPI_SCALE

opts=""
#opts+="--enable-features=UseOzonePlatform "
#opts+="--enable-features=UseOzonePlatform,Vulkan "
#opts+="--enable-features=UseOzonePlatform --ozone-platform=wayland/x11 "
#opts+="--ozone-platform=wayland "
#opts+="--force-device-scale-factor=1.6875 "
#opts+="--force-device-scale-factor=1.6 "
#reload-xrdb
#opts+="--force-device-scale-factor=1.4 "
#opts+="--disable-skia-runtime-opts "
#opts+="--disable-dev-shm-usage "
#opts+="--no-sandbox "
#opts+="--disable-gpu "
#opts+="--log-level=INFO "
opts+="--force-dark-mode "
#opts+="--enable-features=WebUIDarkMode "

# opts+="--disable-zero-copy "
# opts+="--disable-zero-copy-dxgi-video "
opts+="--ignore-gpu-blocklist "
opts+="--enable-zero-copy "
opts+="--enable-gpu-rasterization "
opts+="--enable-pixel-canvas-recording "
# opts+="--enable-accelerated-video-decode "
# opts+="--disable-accelerated-video-decode "
# opts+="--disable-gpu-sandbox "
# opts+="--disable-gpu-compositing "
## opts+="--disable-direct-composition "
## opts+="--disable-direct-composition-video-overlays "
# opts+="--enable-direct-composition "
# opts+="--enable-direct-composition-video-overlays "
# opts+="--disable-running-as-system-compositor "
opts+="--disable-partial-raster "
## opts+="--disable-gpu-memory-buffer-compositor-resources "
#opts+="--disable-gpu-memory-buffer-video-frames "
#opts+="--disable-video-capture-use-gpu-memory-buffer "
#opts+="--gpu-sandbox-start-early "
# opts+="--disable-threaded-compositing "
# opts+="--enable-native-gpu-memory-buffers "
#opts+="--in-process-gpu "
#opts+="--disable-skia-renderer "
#opts+="--enable-skia-renderer "
# opts+="--enable-hardware-overlays "
# opts+="--disable-hardware-overlays "
# opts+="--disable-sgi-video-sync "

#opts+="--use-vulkan "
#opts+="--use-gl=egl "
#opts+="--use-gl=desktop "
#opts+="--use-gl=swiftshader "

# opts+="--chrome-labs "
exec $name $opts
