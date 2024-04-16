#!/usr/bin/env python3

import json, subprocess, sys


## workspace id goes from 1 to 10+


def to_group_id(ws_id):
    return (ws_id - 1) // 10


def to_group_ws_id(ws_id):
    return ws_id - to_group_id(ws_id) * 10


def run_command(args):
    process = subprocess.Popen(args, stdout=subprocess.PIPE, text=True)
    output, err = process.communicate()
    if err:
        print(err, file=sys.stderr)
    return output, (process.returncode == 0)


def run_json_command(args):
    output, success = run_command(args)
    if success:
        return json.loads(output)


def get_workspaces():
    return run_json_command(["hyprctl", "workspaces", "-j"])


def get_active_workspace():
    return run_json_command(["hyprctl", "activeworkspace", "-j"])


def goto_workspace_by_digit(digit_to):
    goto_id = None

    workspaces = get_workspaces()
    active = get_active_workspace()

    id_now = active["id"]
    group_now = to_group_id(id_now)
    digit_now = to_group_ws_id(id_now)

    if digit_to == digit_now:
        # try switching to the other group
        group_to = (group_now + 1) % 2
        id_to = group_to * 10 + digit_to
        ws_to = next((ws for ws in workspaces if ws["id"] == id_to), None)
        if ws_to:
            goto_id = ws_to["id"]
    else:
        # try switching to workspace in current group first
        id_to = (group_now * 10) + digit_to
        ws_to = next(
            (ws for ws in workspaces if ws["id"] == id_to),
            None,
        )
        if ws_to:
            goto_id = ws_to["id"]
        else:
            goto_id = digit_to

    if goto_id:
        _, success = run_command(["hyprctl", "dispatch", "workspace", str(goto_id)])
        return success


if __name__ == "__main__":
    digit_to = int(sys.argv[1])
    success = goto_workspace_by_digit(digit_to)
    if success is False:
        print(f"Failed to switch workspace", file=sys.stderr)
        sys.exit(1)
    else:
        sys.exit(0)
