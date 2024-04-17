#!/usr/bin/env python3

import json, subprocess, sys, os, os.path, socket


class Cache:
    def __init__(self):
        self.cache = {}

    def get(self, key):
        return self.cache.get(key)

    def set(self, key, value):
        self.cache[key] = value

    def clear(self):
        self.cache.clear()


globalCache = Cache()


class Util:
    @staticmethod
    def run_command(args):
        print("Running command:", args, file=sys.stderr)
        process = subprocess.Popen(args, stdout=subprocess.PIPE, text=True)
        output, err = process.communicate()
        if err:
            print(err, file=sys.stderr)
        return output, (process.returncode == 0)

    @staticmethod
    def run_json_command(args, cache=True):
        key = "run_json_command " + " ".join(args)
        saved = globalCache.get(key)
        if cache and saved:
            return saved
        else:
            output, success = Util.run_command(args)
            if success:
                result = json.loads(output)
                globalCache.set(key, result)
                return result
            else:
                raise Exception(f"Command failed: {args}")


class Monitors:
    def __init__(self):
        self.all = Monitors.list()
        self.active = Monitors.get_active()

    @staticmethod
    def list():
        return Util.run_json_command(["hyprctl", "monitors", "-j"], cache=False)

    @staticmethod
    def get_active():
        return Util.run_json_command(["hyprctl", "activemonitor", "-j"], cache=False)

    def find(self, key, value):
        return next((monitor for monitor in self.all if monitor[key] == value), None)

    def get(self, monitor_id):
        return self.find("id", monitor_id)


class Workspaces:
    def __init__(self):
        self.all = Workspaces.list()
        self.active = Workspaces.get_active()

    @staticmethod
    def list():
        return Util.run_json_command(["hyprctl", "workspaces", "-j"], cache=False)

    @staticmethod
    def get_active():
        return Util.run_json_command(["hyprctl", "activeworkspace", "-j"], cache=False)

    @staticmethod
    def to_group_id(ws_id):
        return (ws_id - 1) // 10

    @staticmethod
    def to_group_ws_id(ws_id):
        return ws_id - Workspaces.to_group_id(ws_id) * 10

    def find(self, key, value):
        return next((ws for ws in self.all if ws[key] == value), None)

    def get(self, ws_id):
        return self.find("id", ws_id)


class Wallpapers:
    def __init__(self):
        self.filenames = Wallpapers.list()

    @staticmethod
    def base_dir():
        return os.path.expanduser("~/.local/share/wallpapers")

    @staticmethod
    def active_dir():
        return os.path.join(Wallpapers.base_dir(), "active")

    @staticmethod
    def group_dir():
        return os.path.join(Wallpapers.base_dir(), "groups")

    @staticmethod
    def list():
        return os.listdir(Wallpapers.active_dir())

    @staticmethod
    def for_group(group_id):
        return os.path.join(Wallpapers.base_dir(), "groups", f"{group_id}.png")

    @staticmethod
    def for_workspace(ws_id):
        return Wallpapers.for_group(Workspaces.to_group_id(ws_id))

    def index(self, filename):
        try:
            return self.filenames.index(os.path.basename(filename))
        except ValueError:
            return -1


class Hyprpaper:
    @staticmethod
    def apply(monitor_name, path):
        path = os.path.realpath(os.path.expanduser(path))
        Util.run_command(["hyprctl", "hyprpaper", "preload", path])
        Util.run_command(
            ["hyprctl", "hyprpaper", "wallpaper", f"{monitor_name},{path}"]
        )

    @staticmethod
    def set_for_workspace(ws_id, workspaces=None):
        if workspaces is None:
            workspaces = Workspaces()
        path = Wallpapers.for_workspace(ws_id)
        ws = workspaces.get(ws_id)
        if ws:
            monitor = ws.get("monitor")
            Hyprpaper.apply(monitor, path)

    @staticmethod
    ## Set the wallpaper for any monitors whose active workspace
    ## is in the given group.
    def apply_group(group_id):
        path = Wallpapers.for_group(group_id)
        for monitor in Monitors.list():
            ws_id = monitor["activeWorkspace"]["id"]
            if Workspaces.to_group_id(ws_id) == group_id:
                Hyprpaper.apply(monitor["name"], path)

    @staticmethod
    ## Rewrites the symlink in "groups/{group_id}.png" to point to the
    ## next wallpaper in the list from "active/".
    def cycle_group_wallpaper(group_id):
        wps = Wallpapers()
        print(wps.filenames, file=sys.stderr)
        group_symlink = Wallpapers.for_group(group_id)
        current_path = os.path.realpath(group_symlink)
        current_index = wps.index(current_path)
        next_index = (current_index + 1) % len(wps.filenames)
        next_filename = wps.filenames[next_index]
        next_path = os.path.join(Wallpapers.active_dir(), next_filename)
        os.remove(group_symlink)
        os.symlink(next_path, group_symlink)
        Hyprpaper.apply_group(group_id)

    @staticmethod
    def cycle_focused_wallpaper():
        workspaces = Workspaces()
        ws_id = workspaces.active["id"]
        group_id = Workspaces.to_group_id(ws_id)
        Hyprpaper.cycle_group_wallpaper(group_id)


class Hyprctl:
    @staticmethod
    def dispatch(args):
        return Util.run_command(["hyprctl", "dispatch"] + args)

    @staticmethod
    def goto_workspace(digit_to):
        goto_id = None

        workspaces = Workspaces()

        id_now = workspaces.active["id"]
        group_now = Workspaces.to_group_id(id_now)
        digit_now = Workspaces.to_group_ws_id(id_now)

        if digit_to == digit_now:
            # try switching to the other group
            group_to = (group_now + 1) % 2
            id_to = group_to * 10 + digit_to
            # if workspaces.get(id_to):
            #     goto_id = id_to
            goto_id = id_to
        else:
            # try switching to workspace in current group first
            id_to = (group_now * 10) + digit_to
            ws_to = workspaces.get(id_to)
            if ws_to:
                goto_id = ws_to["id"]
            else:
                goto_id = digit_to

        if goto_id:
            Hyprctl.dispatch(["workspace", str(goto_id)])
            Hyprpaper.set_for_workspace(goto_id)


class HyprSocket2:
    ## This class handles events streamed from the Hyprland socket.
    def __init__(self):
        self.hypr_id = os.getenv("HYPRLAND_INSTANCE_SIGNATURE")
        if not self.hypr_id:
            raise Exception("HYPRLAND_INSTANCE_SIGNATURE not set")
        self.path = f"/tmp/hypr/{self.hypr_id}/.socket2.sock"

    # def connect(self):
    #     self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    #     self.sock.connect(self.path)


if __name__ == "__main__":
    cmd = sys.argv[1]

    if cmd == "goto-workspace":
        Hyprctl.goto_workspace(int(sys.argv[2]))
    elif cmd == "cycle-wallpaper":
        Hyprpaper.cycle_focused_wallpaper()
    elif cmd == "daemon":
        pass
    else:
        print(f"Unknown command: {cmd}", file=sys.stderr)
        sys.exit(1)
