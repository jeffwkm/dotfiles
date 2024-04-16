#!/usr/bin/env python3

# need library for resolving symlinks
import json, subprocess, sys, os, os.path


def resolve_symlink(path):
    return os.path.realpath(path)


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


run_json_command_cache = {}


def run_json_command(args):
    key = " ".join(args)
    cached = run_json_command_cache.get(key)
    if cached:
        return cached
    else:
        output, success = run_command(args)
        if success:
            result = json.loads(output)
            run_json_command_cache[key] = result
            return result


def get_workspaces():
    return run_json_command(["hyprctl", "workspaces", "-j"])


def get_workspace_by_id(ws_id):
    workspaces = get_workspaces()
    return next((ws for ws in workspaces if ws["id"] == ws_id), None)


def get_monitors():
    return run_json_command(["hyprctl", "monitors", "-j"])


def get_active_workspace():
    return run_json_command(["hyprctl", "activeworkspace", "-j"])


wallpapers_dir = os.path.expanduser("~/.local/share/wallpapers")


def list_wallpapers():
    # get list of files in ~/local/share/wallpapers

    os.realpath(wallpapers_dir)
    if not os.path.exists(wallpapers_dir):
        return []
    else:
        return os.listdir(wallpapers_dir)


def get_wallpaper_for_workspace(ws_id):
    path = os.path.expanduser(
        wallpapers_dir + "/groups/" + str(to_group_id(ws_id)) + ".png"
    )
    return os.path.realpath(path)


def set_wallpaper(monitor, path):
    run_command(["hyprctl", "hyprpaper", "preload", path])
    run_command(["hyprctl", "hyprpaper", "wallpaper", f"%s,%s" % (monitor, path)])


def set_wallpaper_for_workspace(ws_id):
    path = get_wallpaper_for_workspace(ws_id)

    monitor = None
    ws = get_workspace_by_id(ws_id)
    return set_wallpaper(ws["monitor"], path)


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
        set_wallpaper_for_workspace(goto_id)
        return success


if __name__ == "__main__":
    cmd = sys.argv[1]

    if cmd == "goto-workspace":
        digit_to = int(sys.argv[2])
        success = goto_workspace_by_digit(digit_to)
        if success is False:
            print(f"Failed to switch workspace", file=sys.stderr)
            sys.exit(1)
        else:
            sys.exit(0)
    else:
        print(f"Unknown command: {cmd}", file=sys.stderr)
        sys.exit(1)
