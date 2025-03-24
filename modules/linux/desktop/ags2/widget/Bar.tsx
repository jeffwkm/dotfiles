import { App, Astal, Gtk, Gdk } from "astal/gtk3";
import Workspaces, { ActiveTitle, hyprland } from "./Workspaces";
import Sound from "./Sound";
import { Date, DateTime, Time } from "./DateTime";
import Tray from "./Tray";
import { bind } from "astal";
import Media from "./Media";
import SystemStatus from "./System";
import { Pomodoro } from "./Tasks";
type BarProps = {
  monitorId: number;
  monitor: Gdk.Monitor;
};

const TopBar = (props: BarProps) => {
  const { monitorId, monitor } = props;
  const { TOP, LEFT, RIGHT } = Astal.WindowAnchor;
  const focused_monitor = bind(hyprland, "focused_monitor");
  const focused = focused_monitor.as((focused_monitor) => focused_monitor.id === monitorId);
  const window_class = focused.as((focused) => `Bar Top ${focused ? "focused" : ""}`);

  return (
    <window
      visible
      name="ags-bar"
      className={window_class}
      monitor={monitorId}
      gdkmonitor={monitor}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      anchor={TOP | LEFT | RIGHT}
      application={App}
      marginBottom={0}
    >
      <centerbox>
        <box className={"Left"} halign={Gtk.Align.START} vexpand valign={Gtk.Align.CENTER}>
          <Workspaces monitorId={monitorId} />
          <ActiveTitle focused={focused} />
        </box>
        <box className={"Center"} halign={Gtk.Align.CENTER} vexpand valign={Gtk.Align.CENTER}></box>
        <box className={"Right"} halign={Gtk.Align.END} vexpand valign={Gtk.Align.CENTER}>
          <Media />
          <Tray />
          <SystemStatus />
          <Sound />
          <DateTime />
        </box>
      </centerbox>
    </window>
  );
};

const BottomBar = (props: BarProps) => {
  const { monitorId, monitor } = props;
  const { BOTTOM, LEFT, RIGHT } = Astal.WindowAnchor;
  const focused_monitor = bind(hyprland, "focused_monitor");
  const focused = focused_monitor.as((focused_monitor) => focused_monitor.id === monitorId);
  const window_class = focused.as((focused) => `Bar Bottom ${focused ? "focused" : ""}`);

  return (
    <window
      visible
      name="ags-bar"
      className={window_class}
      monitor={monitorId}
      gdkmonitor={monitor}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      anchor={BOTTOM | LEFT | RIGHT}
      application={App}
      marginTop={0}
    >
      <centerbox>
        <box className={"Left"} halign={Gtk.Align.START} vexpand valign={Gtk.Align.CENTER}></box>
        <box className={"Center"} halign={Gtk.Align.CENTER} vexpand valign={Gtk.Align.CENTER}>
          <Pomodoro />
        </box>
        <box className={"Right"} halign={Gtk.Align.END} vexpand valign={Gtk.Align.CENTER}></box>
      </centerbox>
    </window>
  );
};

export { TopBar, BottomBar };
