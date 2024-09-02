#!/usr/bin/env bash

export ELECTRON_OZONE_PLATFORM_HINT=auto

appimage="$(ls -1t ~/Downloads/cursor-*.AppImage | head -n1)"

exec "$appimage" "$@"
