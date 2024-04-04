import { Notifications, Widget } from "ags-ts";
const { Box, Icon, Label } = Widget;

export const Notification = () => {
  const popups = Notifications.bind("popups");
  return Box({
    class_name: "notification",
    visible: popups.as((p) => p.length > 0),
    children: [
      Icon({
        icon: "preferences-system-notifications-symbolic",
      }),
      Label({
        label: popups.as((p) => p[0]?.summary || ""),
      }),
    ],
  });
};
