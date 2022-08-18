#!/usr/bin/env bash

set -e

# wait for keys to be released
# sleep 0.05

osascript - <<EOF
if application "Alacritty" is running then
    tell application "Alacritty"
        activate
    end tell
    tell application "System Events"
        keystroke "n" using command down
    end tell
else
   tell application "System Events"
        tell application process "Dock"
            tell list 1
                tell UI element "Alacritty"
                    click
                end tell
            end tell
        end tell
    end tell
end if
EOF
