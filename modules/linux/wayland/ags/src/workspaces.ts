import { Hyprland, Widget, Workspace } from "ags-ts";
const { Box, Button, Label } = Widget;
import { merge } from "resource:///com/github/Aylur/ags/utils.js";

const bindWs = Hyprland.bind("workspaces");
const bindMon = Hyprland.bind("monitors");

export const Workspaces = (monitor: number) => {
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
    return ws.map(({ id, monitorID }) => {
      return Button({
        on_clicked: () => Hyprland.messageAsync(`dispatch workspace ${id}`),
        child: Label({ label: `${id}` }),
        class_name: `${id === first.id ? "first" : ""} ${id === last.id ? "last" : ""} ${id === activeId ? "focused" : ""}`,
      });
    });
  });

  return Box({
    class_name: "workspaces",
    children: workspaces,
  });
};

// TODO: get title for `monitor`
export const ClientTitle = (monitor: number) => {
  return Label({
    class_name: "client-title",
    label: Hyprland.active.client.bind("title"),
  });
};
