import type { Workspace } from "types/service/hyprland";

import { merge, derive, watch, monitorFile } from "ags-ts";

import {
  Hyprland,
  Notifications,
  Mpris,
  Audio,
  Battery,
  SystemTray,
  App,
  Widget,
  Service,
} from "ags-ts";

const timeNow = Variable("", {
  poll: [1000, 'date +"%l:%M %p"'],
});

const dateNow = Variable("", {
  poll: [1000, 'date +"%b %e"'],
});

const bindWs = Hyprland.bind("workspaces");
const bindMon = Hyprland.bind("monitors");

const Workspaces = (monitor: number) => {
  const bindAid = Hyprland.active.workspace.bind("id");

  const binds = merge([bindWs, bindMon, bindAid], (w, m, aid) => [w, m, aid]);

  const workspaces = binds.as(([ws, monitors, aid]) => {
    const activeId = monitors[monitor]?.activeWorkspace.id;
    // const activeId = hyprland.getMonitor(monitor)?.activeWorkspace.id;
    ws = ws.filter((w) => w.monitorID === monitor);
    ws.sort((a, b) => a.id - b.id);
    ws.sort((a, b) => a.monitorID - b.monitorID);
    const first = ws[0];
    const last = ws[ws.length - 1];
    return ws.map(({ id, monitorID }: Workspace) => {
      return Widget.Button({
        on_clicked: () => Hyprland.messageAsync(`dispatch workspace ${id}`),
        child: Widget.Label({ label: `${id}` }),
        class_name: `${id === first.id ? "first" : ""} ${id === last.id ? "last" : ""} ${id === activeId ? "focused" : ""}`,
      });
    });
  });

  return Widget.Box({
    class_name: "workspaces",
    children: workspaces,
  });
};

const ClientTitle = (monitor: number) => {
  return Widget.Label({
    class_name: "client-title",
    label: Hyprland.active.client.bind("title"),
  });
};

const Clock = () => {
  return Widget.Box({
    class_name: "datetime",
    children: [
      Widget.Label({
        class_name: "date",
        label: dateNow.bind(),
      }),
      Widget.Label({
        class_name: "time",
        label: timeNow.bind(),
      }),
    ],
  });
};

// we don't need dunst or any other notification daemon
// because the Notifications module is a notification daemon itself
const Notification = () => {
  const popups = Notifications.bind("popups");
  return Widget.Box({
    class_name: "notification",
    visible: popups.as((p) => p.length > 0),
    children: [
      Widget.Icon({
        icon: "preferences-system-notifications-symbolic",
      }),
      Widget.Label({
        label: popups.as((p) => p[0]?.summary || ""),
      }),
    ],
  });
};

const Media = () => {
  const label = watch("", Mpris, "player-changed", () => {
    if (Mpris.players[0]) {
      const { track_artists, track_title } = Mpris.players[0];
      return `${track_artists.join(", ")} - ${track_title}`;
    } else {
      return "Nothing is playing";
    }
  });

  return Widget.Button({
    class_name: "media",
    on_primary_click: () => Mpris.getPlayer("")?.playPause(),
    on_scroll_up: () => Mpris.getPlayer("")?.next(),
    on_scroll_down: () => Mpris.getPlayer("")?.previous(),
    child: Widget.Label({ label }),
  });
};

const Volume = () => {
  const icons = {
    101: "overamplified",
    67: "high",
    34: "medium",
    1: "low",
    0: "muted",
  };

  const getIcon = () => {
    const icon = Audio.speaker.is_muted
      ? 0
      : [101, 67, 34, 1, 0].find((threshold) => threshold <= Audio.speaker.volume * 100);

    return `audio-volume-${icons[icon]}-symbolic`;
  };

  const icon = Widget.Icon({
    class_name: "icon",
    icon: watch(getIcon(), Audio.speaker, getIcon),
  });

  const status = Widget.CircularProgress({
    class_name: "circular",
    value: Audio.speaker.bind("volume").as((v) => v || 0),
  });

  return Widget.Box({
    class_name: "volume",
    children: [icon, status],
  });
};

const BatteryLabel = () => {
  const value = Battery.bind("percent").as((p) => (p > 0 ? p / 100 : 0));
  const icon = Battery.bind("percent").as(
    (p) => `battery-level-${Math.floor(p / 10) * 10}-symbolic`,
  );

  return Widget.Box({
    class_name: "battery",
    visible: Battery.bind("available"),
    children: [
      Widget.Icon({ icon }),
      Widget.LevelBar({
        widthRequest: 140,
        vpack: "center",
        value,
      }),
    ],
  });
};

const SysTray = () => {
  const items = SystemTray.bind("items").as((items) =>
    items.map((item) =>
      Widget.Button({
        child: Widget.Icon({ icon: item.bind("icon") }),
        on_primary_click: (_, event) => item.activate(event),
        on_secondary_click: (_, event) => item.openMenu(event),
        tooltip_markup: item.bind("tooltip_markup"),
      }),
    ),
  );

  return Widget.Box({
    class_name: "systray",
    children: items,
  });
};

const Left = (monitor: number) =>
  Widget.Box({
    spacing: 6,
    children: [Workspaces(monitor), ClientTitle(monitor)],
  });

const Center = (monitor: number) =>
  Widget.Box({
    spacing: 8,
    // children: [Media(), Notification()],
    children: [Notification()],
  });

const Right = (monitor: number) =>
  Widget.Box({
    hpack: "end",
    spacing: 4,
    children: [SysTray(), Volume(), BatteryLabel(), Clock()],
  });

const Bar = (monitor: number) => {
  const box_css = Hyprland.active
    .bind("monitor")
    .as((mon) => `outer ${monitor === mon.id ? "active" : ""}`);
  return Widget.Window({
    name: `ags-${monitor}`, // name has to be unique
    class_name: "bar",
    monitor,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    child: Widget.CenterBox({
      start_widget: Left(monitor),
      center_widget: Center(monitor),
      end_widget: Right(monitor),
      class_name: box_css,
    }),
  });
};

const cssOut = `${App.configDir}/style.css`;

App.config({
  gtkTheme: "Adwaita-dark",
  style: cssOut,
  // windows: [Bar(0), Bar(1)],
  windows: [Bar(0)],
});

monitorFile(cssOut, () => {
  App.resetCss();
  App.applyCss(cssOut);
});
