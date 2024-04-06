#!/usr/bin/env bash
set -u

# add scripts for sway environment to path
scripts="$(fd sway/scripts ~/.config/nixpkgs/modules -E result -p -t d)"
export PATH="$scripts:$PATH"

# configure cursor
export XCURSOR_SIZE=24
export XCURSOR_THEME="capitaine-cursors-white"

# ensure ssh key is loaded
have_ssh_id() {
  ssh-add -l | grep -q "$USER"
}

while ! have_ssh_id; do
  echo "ssh key must be added before starting hyprland"
  ssh-add || (echo "Failed to add ssh key" && true)
done

# load user environment
# systemctl --user import-environment

(mount | grep /mnt/huge 2>/dev/null) || mount /mnt/huge

# run hyprland with systemd logging
# systemd-cat --identifier=hyprland Hyprland
Hyprland

# terminate user session upon hyprland exit
systemctl --user stop graphical-session.target
loginctl terminate-user $UID
