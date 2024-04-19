import Hyprland from "resource:///com/github/Aylur/ags/service/hyprland.js";
import {
  SysTray,
  CpuTemp,
  Volume,
  BatteryLabel,
  Clock,
  Workspaces,
  ClientTitle,
  Notification,
  Media,
  Pomodoro,
  CpuGroup,
} from "./widgets";

const TopLeft = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-left",
    children: [Workspaces(monitorId), ClientTitle(monitorId)],
  });

const TopCenter = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-center",
    hpack: "center",
    children: [Media()],
    // children: [],
  });

const TopRight = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-right",
    hpack: "end",
    children: [Notification(), , SysTray(), CpuGroup(), Volume(), BatteryLabel(), Clock()],
  });

const BottomCenter = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-center",
    children: [Pomodoro()],
  });

export const TopBar = (monitorId: number) =>
  Widget.Window({
    name: `ags-top-${monitorId}`, // name has to be unique
    class_name: "bar bar-top",
    monitor: monitorId,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    child: Widget.CenterBox({
      start_widget: TopLeft(monitorId),
      center_widget: TopCenter(monitorId),
      end_widget: TopRight(monitorId),
      class_name: "outer",
      setup: (self) => {
        self.hook(Hyprland.active.monitor, (self) => {
          const isActive = monitorId === Hyprland.active.monitor.id;
          self.class_name = `outer ${isActive ? "active" : ""}`;
        });
      },
    }),
  });

export const BottomBar = (monitorId: number) =>
  Widget.Window({
    name: `ags-bottom-${monitorId}`, // name has to be unique
    class_name: "bar bar-bottom",
    monitor: monitorId,
    anchor: ["bottom", "left", "right"],
    exclusivity: "exclusive",
    child: Widget.CenterBox({
      start_widget: Widget.Box({}),
      center_widget: BottomCenter(monitorId),
      end_widget: Widget.Box({}),
      class_name: "outer",
      setup: (self) => {
        self.hook(Hyprland.active.monitor, (self) => {
          const isActive = monitorId === Hyprland.active.monitor.id;
          self.class_name = `outer ${isActive ? "active" : ""}`;
        });
      },
    }),
  });
