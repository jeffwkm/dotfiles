#!/usr/bin/env bash
# set -euxo pipefail

sudo journalctl --system -fb --no-tail
exit 0

sudo journalctl --system -fb --no-tail |
    grep -v 'pam_unix(sudo:session)' |
    grep -v DDC
