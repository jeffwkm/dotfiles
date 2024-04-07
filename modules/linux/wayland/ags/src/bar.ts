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
} from "./widgets";

const Left = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-left",
    children: [Workspaces(monitorId), ClientTitle(monitorId)],
  });

const Center = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-center",
    children: [Media(), Notification()],
  });

const Right = (monitorId: number) =>
  Widget.Box({
    class_name: "bar-right",
    hpack: "end",
    children: [SysTray(), CpuTemp(), Volume(), BatteryLabel(), Clock()],
  });

export const Bar = (monitorId: number) =>
  Widget.Window({
    name: `ags-${monitorId}`, // name has to be unique
    class_name: "bar",
    monitor: monitorId,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    child: Widget.CenterBox({
      start_widget: Left(monitorId),
      center_widget: Center(monitorId),
      end_widget: Right(monitorId),
      class_name: "outer",
      setup: (self) => {
        self.hook(Hyprland.active.monitor, (self) => {
          const isActive = monitorId === Hyprland.active.monitor.id;
          self.class_name = `outer ${isActive ? "active" : ""}`;
        });
      },
    }),
  });

export default Bar;
