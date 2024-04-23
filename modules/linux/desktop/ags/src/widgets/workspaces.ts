import Hyprland from "resource:///com/github/Aylur/ags/service/hyprland.js";
import Widget from "resource:///com/github/Aylur/ags/widget.js";

const toGroupId = (wsId: number) => ((wsId - 1) / 10) >> 0;
const toGroupWsId = (wsId: number) => wsId - toGroupId(wsId) * 10;

export const ClientTitle = (monitorId: number) => {
  const title = Variable("");
  return Widget.Box({
    class_name: "client-title",
    visible: title.bind().as((t) => t !== ""),
    children: [
      Widget.Label({
        maxWidthChars: 60,
        ellipsize: 3,
        label: title.bind(),
        setup: (self) => {
          self.hook(Hyprland.active.client, (self) => {
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
          .sort((a, b) => a.id - b.id);
        const first = ws[0];
        const last = ws[ws.length - 1];
        // const groupSyms = ["", "+", "*"];
        const groupSyms = ["", "", ""];
        self.visible = ws.length > 0;
        self.children = ws.map(({ id }) =>
          Widget.Button({
            class_name: `${id === first.id ? "first" : ""} ${id === last.id ? "last" : ""} ${id === activeId ? "focused" : ""}`,
            label: `${groupSyms[toGroupId(id)]}${toGroupWsId(id) % 10}`,
            vexpand: false,
            vpack: "center",
            hpack: "center",
            on_clicked: () => Hyprland.messageAsync(`dispatch workspace ${id}`),
          }),
        );
      });
    },
  });
};

// TODO: don't create workspacesGroup if there are no workspaces
export const Workspaces = (monitorId: number) =>
  Widget.Box({
    class_name: "workspaces",
    children: [0, 1].map((groupId) => workspacesGroup(monitorId, groupId)),
  });
