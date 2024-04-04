import { Widget, Audio, Battery, SystemTray } from "ags-ts";
const { LevelBar, Icon, Box, Button, Label, CircularProgress } = Widget;
import { watch } from "resource:///com/github/Aylur/ags/utils.js";

const timeNow = Variable("", {
  poll: [1000, 'date +"%l:%M %p"'],
});

const dateNow = Variable("", {
  poll: [1000, 'date +"%b %e"'],
});

export const Clock = () => {
  return Box({
    class_name: "datetime",
    children: [
      Label({
        class_name: "date",
        label: dateNow.bind(),
      }),
      Label({
        class_name: "time",
        label: timeNow.bind(),
      }),
    ],
  });
};

export const Volume = () => {
  const icons = {
    101: "overamplified",
    67: "high",
    34: "medium",
    1: "low",
    0: "muted",
  };

  const getIcon = () => {
    const icon = Audio.speaker.is_muted
      ? 0
      : [101, 67, 34, 1, 0].find((threshold) => threshold <= Audio.speaker.volume * 100);

    return `audio-volume-${icons[icon]}-symbolic`;
  };

  const icon = Icon({
    class_name: "icon",
    icon: watch(getIcon(), Audio.speaker, getIcon),
  });

  const status = CircularProgress({
    class_name: "circular",
    value: Audio.speaker.bind("volume").as((v) => v || 0),
  });

  return Box({
    class_name: "volume",
    children: [icon, status],
  });
};

export const BatteryLabel = () => {
  const icons = {
    98: "full",
    50: "good",
    20: "low",
    1: "caution",
    0: "empty",
  };

  const getIcon = () => {
    if (!Battery.available) {
      return "battery-missing";
    }
    const level = [98, 50, 20, 1, 0].find((threshold) => threshold <= Battery.percent);
    const charging = Battery.charging && Battery.percent >= 1 ? "-charging" : "";
    return `battery-${icons[level]}${charging}`;
  };

  return Box({
    class_name: "battery",
    visible: Battery.bind("available"),
    children: [
      Icon({
        class_name: "icon",
        icon: watch(getIcon(), Battery, getIcon),
      }),
      LevelBar({
        widthRequest: 140,
        vpack: "center",
        value: Battery.bind("percent").as((p) => (p && p > 0 ? p : 0)),
      }),
    ],
  });
};

export const SysTray = () => {
  const items = SystemTray.bind("items").as((items) =>
    items.map((item) =>
      Button({
        child: Icon({ icon: item.bind("icon") }),
        on_primary_click: (_, event) => item.activate(event),
        on_secondary_click: (_, event) => item.openMenu(event),
        tooltip_markup: item.bind("tooltip_markup"),
      }),
    ),
  );

  return Box({
    class_name: "systray",
    children: items,
  });
};
