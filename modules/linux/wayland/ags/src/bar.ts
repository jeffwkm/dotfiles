import { Hyprland, Widget } from "ags-ts";
const { Box, CenterBox, Window } = Widget;

import { SysTray, Volume, BatteryLabel, Clock } from "./status";
import { Workspaces, ClientTitle } from "./workspaces";
import { Notification } from "./notification";

const Left = (monitor: number) =>
  Box({
    spacing: 6,
    children: [Workspaces(monitor), ClientTitle(monitor)],
  });

const Center = (monitor: number) =>
  Box({
    spacing: 8,
    // children: [Media(), Notification()],
    children: [Notification()],
  });

const Right = (monitor: number) =>
  Box({
    hpack: "end",
    spacing: 4,
    children: [SysTray(), Volume(), BatteryLabel(), Clock()],
  });

export const Bar = (monitor: number) => {
  const box_css = Hyprland.active
    .bind("monitor")
    .as((mon) => `outer ${monitor === mon.id ? "active" : ""}`);
  return Window({
    name: `ags-${monitor}`, // name has to be unique
    class_name: "bar",
    monitor,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    child: CenterBox({
      start_widget: Left(monitor),
      center_widget: Center(monitor),
      end_widget: Right(monitor),
      class_name: box_css,
    }),
  });
};
