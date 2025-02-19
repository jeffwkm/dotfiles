import { App, Astal, Gtk, Gdk } from "astal/gtk3";
import Workspaces, { ActiveTitle } from "./Workspaces";
import { Date, Time } from "./DateTime";
import Tray from "./Tray";
type BarProps = {
  monitorId: number;
  monitor: Gdk.Monitor;
};

const Bar = (props: BarProps) => {
  const { monitorId, monitor } = props;
  const { TOP, LEFT, RIGHT } = Astal.WindowAnchor;

  return (
    <window
      visible
      name="ags-bar"
      className={"Bar"}
      monitor={monitorId}
      gdkmonitor={monitor}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      anchor={TOP | LEFT | RIGHT}
      application={App}
      marginBottom={8}
    >
      <centerbox>
        <box className={"Left"} hexpand halign={Gtk.Align.START} valign={Gtk.Align.CENTER}>
          <Workspaces monitorId={monitorId} />
        </box>
        <box className={"Center"} vexpand valign={Gtk.Align.CENTER}>
          <ActiveTitle />
        </box>
        <box
          className={"Right"}
          hexpand
          halign={Gtk.Align.END}
          vexpand
          // valign={Gtk.Align.CENTER}
        >
          <box valign={Gtk.Align.CENTER}>
            <Tray />
          </box>
          <box valign={Gtk.Align.CENTER}>
            <Date />
            <Time />
          </box>
        </box>
      </centerbox>
      {/* <centerbox cssName="centerbox">
        <button onClicked="echo hello" hexpand halign={Gtk.Align.CENTER}>
          Welcome to AGS!
        </button>
        <box />
        <menubutton hexpand halign={Gtk.Align.CENTER}>
          <label label={time()} />
          <popover>
            <Gtk.Calendar />
          </popover>
        </menubutton>
      </centerbox> */}
    </window>
  );
};

export default Bar;
