import { bind, Variable } from "astal";
import { Gtk } from "astal/gtk3";
import Hyprland from "gi://AstalHyprland?version=0.1";
import Pango from "gi://Pango?version=1.0";

const hyprland = Hyprland.get_default();

type WorkspaceItem = {
  id: number;
  groupId: number;
  monitorId: number;
};

const sortById = (ws1: WorkspaceItem, ws2: WorkspaceItem) => ws1.id - ws2.id;

const getWorkspaceItems = (workspaces: Hyprland.Workspace[]): WorkspaceItem[] =>
  workspaces
    .map((workspace) => {
      const monitor = workspace.get_monitor();
      if (!monitor) return null;
      const id = workspace.get_id();
      const groupId = Math.floor((id - 1) / 10);
      const monitorId = monitor.get_id();
      return {
        id,
        groupId,
        monitorId,
      };
    })
    .filter((item) => item !== null)
    .sort(sortById);

type WorkspaceProps = {} & WorkspaceItem;

const Workspace = (props: WorkspaceProps) => {
  const { id, monitorId } = props;

  const monitor = hyprland.get_monitor(monitorId);
  if (!monitor) return null;

  const active = Variable(monitor.get_active_workspace()?.get_id() === id ? "active" : "");
  bind(monitor, "active_workspace").subscribe((active_workspace) => {
    active.set(active_workspace?.get_id() === id ? "active" : "");
  });

  const focused = Variable(hyprland.get_focused_monitor()?.get_id() === monitorId ? "focused" : "");
  bind(hyprland, "focused_monitor").subscribe((focused_monitor) => {
    focused.set(focused_monitor?.get_id() === monitorId ? "focused" : "");
  });

  const classes = Variable.derive([focused, active], (focused, active) =>
    ["Workspace", active, focused].filter((x) => x.length > 0)
  );

  return <box className={classes().as((c) => c.join(" "))}>{String(id % 10)}</box>;
};

type WorkspaceGroupProps = {
  groupId: number;
  monitorId: number;
  items: WorkspaceItem[];
};

const WorkspaceGroup = (props: WorkspaceGroupProps) => {
  const { items, monitorId, groupId } = props;

  const visibleItems = items
    .filter((item) => item.monitorId === monitorId)
    .filter((item) => item.groupId === groupId)
    .sort(sortById);

  if (visibleItems.length === 0) return null;

  return (
    <box className={"WorkspaceGroup section"}>
      {visibleItems.map((item) => (
        <Workspace {...item} />
      ))}
    </box>
  );
};

type WorkspacesProps = {
  monitorId: number;
};

const Workspaces = (props: WorkspacesProps) => {
  const { monitorId } = props;

  const items = Variable(getWorkspaceItems(hyprland.get_workspaces()));

  hyprland.connect("event", (hyprland, event, _args) => {
    // console.log(event, args);
    if (event.includes("workspace") || event.includes("focused") || event.includes("monitor")) {
      items.set(getWorkspaceItems(hyprland.get_workspaces()));
    }
  });

  return (
    <box className={"Workspaces"} vexpand={true} valign={Gtk.Align.CENTER}>
      {items((items) => (
        <>
          <WorkspaceGroup groupId={0} monitorId={monitorId} items={items} />
          <WorkspaceGroup groupId={1} monitorId={monitorId} items={items} />
        </>
      ))}
    </box>
  );
};

const ActiveTitle = () => {
  const text = Variable(hyprland.get_focused_client()?.get_title() ?? "");

  const client = bind(hyprland, "focused_client");

  client.subscribe((client) => {
    if (client) {
      text.set(client.get_title());
      bind(client, "title").subscribe((title) => {
        text.set(title);
      });
    } else {
      text.set("");
    }
  });

  return (
    <box
      className={"ActiveTitle section"}
      hexpand={true}
      halign={Gtk.Align.CENTER}
      visible={text((text) => text.length > 0)}
    >
      <label
        maxWidthChars={80}
        ellipsize={Pango.EllipsizeMode.END}
        label={text()}
        hexpand={true}
        halign={Gtk.Align.CENTER}
      />
    </box>
  );
};

export { ActiveTitle };

export default Workspaces;
