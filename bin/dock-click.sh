#!/usr/bin/env bash

script="
on run argv
   tell application \"System Events\"
        tell application process \"Dock\"
            tell list 1
                tell UI element \"$1\"
                    click
                end tell
            end tell
        end tell
    end tell
    return \"\"
end run
"

echo "$script" | osascript
