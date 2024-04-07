import Audio from "resource:///com/github/Aylur/ags/service/audio.js";
import Battery from "resource:///com/github/Aylur/ags/service/battery.js";
import SystemTray from "resource:///com/github/Aylur/ags/service/systemtray.js";

const timeNow = Variable("", {
  poll: [1000, 'date +"%l:%M %p"'],
});

const dateNow = Variable("", {
  poll: [5000, 'date +"%b %e"'],
});

const cpuTemp = Variable(0, {
  poll: [
    1500,
    (_) => {
      const temp = Utils.readFile("/sys/class/hwmon/hwmon4/temp1_input");
      if (!temp) {
        return 0;
      }
      return parseInt(temp) / 1000;
    },
  ],
});

export const Clock = () => {
  return Widget.Box({
    class_name: "datetime",
    children: [
      Widget.Label({
        class_name: "date",
        label: dateNow.bind(),
      }),
      Widget.Label({
        class_name: "time",
        label: timeNow.bind(),
      }),
    ],
  });
};

export const CpuTemp = () => {
  const levels = {
    90: "critical",
    80: "high",
    65: "medium",
    0: "low",
  };

  const getLevel = (t) => {
    const threshold = [90, 80, 65, 0].find((threshold) => threshold <= t);
    return levels[threshold];
  };

  const icon = Widget.Icon({
    class_name: "icon",
    icon: "sensors-temperature-symbolic",
  });

  const label = Widget.Label({
    label: cpuTemp.bind().as((t) => `${Math.floor(t)}°`),
    halign: "end",
  });

  return Widget.Box({
    class_name: cpuTemp.bind().as((t) => `cputemp ${getLevel(t)}`),
    visible: cpuTemp.bind().as((t) => t > 0),
    children: [icon, label],
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

  const icon = Widget.Icon({
    class_name: "icon",
    icon: Utils.watch(getIcon(), Audio.speaker, getIcon),
  });

  const status = Widget.CircularProgress({
    class_name: "circular",
    value: Audio.speaker.bind("volume").as((v) => v || 0),
  });

  return Widget.Box({
    class_name: "volume",
    children: [icon, Widget.Box({ children: [status] })],
  });
};

export const BatteryLabel = () => {
  const available = Battery.bind("available");
  const percent = Battery.bind("percent").as((p) => (p > 0 ? p / 100 : 0));
  const charging = Battery.bind("charging").as((c) => (c ? "-charging" : ""));
  const icon = Utils.merge([available, percent, charging], (a, p, c) =>
    available ? `battery-level-${Math.floor(p * 10) * 10}-symbolic${c}` : "battery-missing",
  );

  return Widget.Box({
    class_name: "battery",
    visible: available,
    children: [Widget.Icon({ icon, class_name: "icon" })],
  });
};

// export const BatteryLabel = () => {
//   const icons = {
//     98: "full",
//     50: "good",
//     20: "low",
//     1: "caution",
//     0: "empty",
//   };

//   const getIcon = () => {
//     if (!Battery.available) {
//       return "battery-missing";
//     }
//     const level = [98, 50, 20, 1, 0].find((threshold) => threshold <= Battery.percent);
//     const charging = Battery.charging && Battery.percent >= 1 ? "-charging" : "";
//     return `battery-${icons[level]}${charging}`;
//   };

//   return Box({
//     class_name: "battery",
//     visible: Battery.bind("available"),
//     children: [
//       Icon({
//         class_name: "icon",
//         icon: Utils.watch(getIcon(), Battery, getIcon),
//       }),
//       LevelBar({
//         widthRequest: 140,
//         vpack: "center",
//         value: Battery.bind("percent").as((p) => (p && p > 0 ? p : 0)),
//       }),
//     ],
//   });
// };

export const SysTray = () => {
  const items = SystemTray.bind("items").as((items) =>
    items.map((item) =>
      Widget.Button({
        child: Widget.Icon({ icon: item.bind("icon") }),

        on_primary_click: (_, event) => item.activate(event),
        on_secondary_click: (_, event) => item.openMenu(event),
        tooltip_markup: item.bind("tooltip_markup"),
      }),
    ),
  );

  return Widget.Box({
    class_name: "systray",
    children: items,
  });
};
