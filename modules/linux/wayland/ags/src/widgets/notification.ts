import Notifications from "resource:///com/github/Aylur/ags/service/notifications.js";

Notifications.popupTimeout = 3000;

export const Notification = () => {
  const popups = Notifications.bind("popups");
  return Widget.Box({
    class_name: "notification",
    visible: popups.as((p) => p.length > 0),
    children: [
      Widget.Icon({
        icon: "preferences-system-notifications-symbolic",
      }),
      Widget.Label({
        label: popups.as((p) => p[0]?.summary || ""),
      }),
    ],
  });
};

export default Notification;
