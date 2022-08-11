#!/usr/bin/env bash
# set -euxo pipefail

journalctl --user -fb --no-tail |
    grep -v 'pam_unix(sudo:session)' |
    grep -v 'libpng warning: iCCP'

exit 0

journalctl --user -fb --no-tail |
    grep -v 'Handling command' |
    grep -v ddcutil |
    grep -v 'pam_unix(sudo:session)' |
    grep -v 'Unsupported maximum keycode' |
    grep -v 'Errors from xkbcomp are not fatal' |
    grep -v 'Could not resolve keysym' |
    grep -v 'The XKEYBOARD keymap compiler (xkbcomp) reports' |
    grep -v 'X11 cannot support keycodes above 255' |
    grep -v 'libpng warning: iCCP: known incorrect sRGB profile' |
    grep -v 'Ignored unknown option' |
    grep -v 'Theme parsing error'

# --identifier=sway
