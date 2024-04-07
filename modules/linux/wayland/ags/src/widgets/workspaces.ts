import Hyprland from "resource:///com/github/Aylur/ags/service/hyprland.js";

const toGroupId = (wsId: number) => ((wsId - 1) / 10) >> 0;
const toGroupWsId = (wsId: number) => wsId - toGroupId(wsId) * 10;

export const ClientTitle = (monitorId: number) => {
  const title = Variable("");
  return Widget.Box({
    class_name: "client-title",
    halign: "start",
    visible: title.bind().as((t) => t !== ""),
    children: [
      Widget.Label({
        halign: "start",
        justification: "left",
        truncate: "end",
        label: title.bind(),
        setup: (self) => {
          self.hook(Hyprland.active.client, (self: Widget.Label) => {
            const mon = Hyprland.monitors[monitorId];
            const ws = mon?.activeWorkspace;
            title.setValue(Hyprland.active.client.title || "");
          });
        },
      }),
    ],
  });
};

const workspacesGroup = (monitorId: number, groupId: number) => {
  return Widget.Box({
    class_name: "workspaces-group",
    visible: false,
    setup: (self) => {
      self.hook(Hyprland, (self) => {
        const activeId = Hyprland.monitors[monitorId]?.activeWorkspace.id;
        const allWss = Hyprland.workspaces;
        const ws = allWss
          .filter((w) => w.monitorID === monitorId)
          .filter((w) => toGroupId(w.id) === groupId)
          .sort((a, b) => a.id - b.id)
          .sort((a, b) => a.monitorID - b.monitorID);
        const first = ws[0];
        const last = ws[ws.length - 1];
        const groupSyms = ["", "+", "*"];
        self.visible = ws.length > 0;
        self.children = ws.map(({ id }) =>
          Widget.Button({
            on_clicked: () => Hyprland.messageAsync(`dispatch workspace ${id}`),
            child: Widget.Label({ label: `${groupSyms[toGroupId(id)]}${toGroupWsId(id)}` }),
            class_name: `${id === first.id ? "first" : ""} ${id === last.id ? "last" : ""} ${id === activeId ? "focused" : ""}`,
          }),
        );
      });
    },
  });
};

export const Workspaces = (monitorId: number) =>
  Widget.Box({
    class_name: "workspaces",
    children: [0, 1].map((groupId) => workspacesGroup(monitorId, groupId)),
  });