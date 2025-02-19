import AstalTray from "gi://AstalTray?version=0.1";
import { bind } from "astal/binding";
import { Gtk } from "astal/gtk3";

const tray = AstalTray.get_default();

const TrayItem = (props: { item: AstalTray.TrayItem }) => {
  const { item } = props;
  return (
    <menubutton
      className={"TrayItem"}
      tooltipMarkup={bind(item, "tooltipMarkup")}
      usePopover={false}
      actionGroup={bind(item, "actionGroup").as((ag) => ["dbusmenu", ag])}
      menuModel={bind(item, "menuModel")}
      direction={Gtk.ArrowType.DOWN}
    >
      <icon gicon={bind(item, "gicon")} />
    </menubutton>
  );
};

const Tray = () => {
  const items = bind(tray, "items");
  return (
    <box className={"Tray section"} visible={items.as((items) => items.length > 0)}>
      {items.as((items) => items.map((item) => <TrayItem item={item} />))}
    </box>
  );
};

export default Tray;
