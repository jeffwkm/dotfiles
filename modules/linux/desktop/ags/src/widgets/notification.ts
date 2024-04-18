import Notifications from "resource:///com/github/Aylur/ags/service/notifications.js";

Notifications.popupTimeout = 2000;

export const Notification = () => {
  const popups = Notifications.bind("popups");
  return Widget.Box({
    class_name: "notification",
    visible: popups.as((p) => p.length > 0),
    children: [
      Widget.Icon({
        icon: "preferences-system-notifications-symbolic",
      }),
      Widget.Button({
        label: popups.as((p) => p[0]?.summary || ""),
        on_clicked: () => Notifications.popups[0]?.close(),
      }),
    ],
  });
};

export default Notification;
