#!/usr/bin/env bash

export ELECTRON_OZONE_PLATFORM_HINT=auto

appimage="$(ls -1t ~/Downloads/Cursor-*.AppImage | head -n1)"

chmod +x "$appimage"

exec "$appimage" "$@"
