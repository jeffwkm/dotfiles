import Audio from "resource:///com/github/Aylur/ags/service/audio.js";
import Battery from "resource:///com/github/Aylur/ags/service/battery.js";
import SystemTray from "resource:///com/github/Aylur/ags/service/systemtray.js";

const timeNow = Variable("", {
  poll: [1000, 'date +"%l:%M %p"'],
});

const dateNow = Variable("", {
  poll: [5000, 'date +"%a %b %e"'],
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

  const getLevel = (t: number) => {
    const threshold = [90, 80, 65, 0].find((threshold) => threshold <= t) || 0;
    return levels[threshold];
  };

  const icon = Widget.Icon({
    class_name: "icon",
    icon: "sensors-temperature-symbolic",
  });

  const label = Widget.Label({
    label: cpuTemp.bind().as((t) => `${Math.floor(t)}°`),
  });

  return Widget.Box({
    class_name: cpuTemp.bind().as((t) => `cputemp ${getLevel(t)}`),
    visible: cpuTemp.bind().as((t) => t > 0),
    children: [icon, label],
  });
};

const volumeIcons = {
  101: "overamplified",
  67: "high",
  34: "medium",
  1: "low",
  0: "muted",
};

const getVolumeIcon = () => {
  const threshold = Audio.speaker.is_muted
    ? 0
    : [101, 67, 34, 1, 0].find((t) => t <= Audio.speaker.volume * 100) || 0;
  return `audio-volume-${volumeIcons[threshold]}-symbolic`;
};

export const Volume = () =>
  Widget.Box({
    class_name: "volume",
    children: [
      Widget.Icon({
        class_name: "icon",
        setup: (self) => {
          self.hook(Audio.speaker, (self) => {
            self.icon = getVolumeIcon();
          });
        },
      }),
      Widget.Box({
        class_name: "status",
        child: Widget.Box({
          child: Widget.CircularProgress({
            class_name: "circular",
            value: Audio.speaker.bind("volume").as((v) => v || 0),
          }),
        }),
      }),
    ],
  });

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

export const SysTray = () =>
  Widget.Box({
    class_name: "systray",
    children: SystemTray.bind("items").as((items) =>
      items.map((item) =>
        Widget.Button({
          child: Widget.Icon({ class_name: "icon", icon: item.bind("icon") }),
          on_primary_click: (_, event) => item.activate(event),
          on_secondary_click: (_, event) => item.openMenu(event),
          tooltipMarkup: item.bind("tooltip_markup"),
        }),
      ),
    ),
  });
