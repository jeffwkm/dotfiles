#!/usr/bin/env bash
set -euo pipefail

CURSOR_SETTINGS="
Xcursor.theme: $XCURSOR_THEME
Xcursor.size: $XCURSOR_SIZE
"

echo "$CURSOR_SETTINGS" | xrdb -merge > /dev/null 2>&1

# vim:ft=sh:
