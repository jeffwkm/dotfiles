import Audio from "resource:///com/github/Aylur/ags/service/audio.js";
import Battery from "resource:///com/github/Aylur/ags/service/battery.js";
import SystemTray from "resource:///com/github/Aylur/ags/service/systemtray.js";

const hostName = Utils.exec("hostname").trim();

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
      let temp;
      if (hostName === "jeff-nixos") {
        temp = Utils.readFile("/sys/class/hwmon/hwmon4/temp1_input");
      }
      if (!temp) {
        return 0;
      }
      return parseInt(temp) / 1000;
    },
  ],
});

const avgList = (list: number[]) => list.reduce((a, b) => a + b, 0) / list.length;

const getCpuMhzAll = () =>
  Utils.readFile("/proc/cpuinfo")
    .split("\n")
    .map((line) => {
      if (line.startsWith("cpu MHz")) {
        return parseFloat(line.split(":")[1].trim());
      } else {
        return null;
      }
    })
    .filter((x) => x);

const getCpuMhzAvg = () => avgList(getCpuMhzAll());

const cpuMhz = Variable(0, {
  poll: [
    1500,
    (_) => {
      let speed;
      speed = getCpuMhzAvg();
      if (!speed || speed === "") {
        return 0;
      }
      return Math.floor(speed);
    },
  ],
});

export const Clock = () =>
  Widget.Box({
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

export const CpuSpeed = () =>
  Widget.Box({
    class_name: "cpu-speed",
    children: [
      // Widget.Icon({
      //   class_name: "icon",
      //   icon: "computer-symbolic",
      // }),
      Widget.Label({
        class_name: "mhz",

        visible: cpuMhz.bind().as((mhz) => mhz > 0),
        label: cpuMhz.bind().as((mhz) => `${mhz} ㎒`),
      }),
    ],
  });

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

  return Widget.Box({
    class_name: cpuTemp.bind().as((t) => `cputemp ${getLevel(t)}`),
    visible: cpuTemp.bind().as((t) => t > 0),
    children: [
      Widget.Label({
        label: cpuTemp.bind().as((t) => `${Math.floor(t)}°`),
      }),
      Widget.Icon({
        class_name: "icon",
        icon: "sensors-temperature-symbolic",
      }),
    ],
  });
};

export const CpuGroup = () =>
  Widget.Box({
    class_name: "cpu-group",
    children: [CpuSpeed(), CpuTemp()],
  });

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
      Widget.Box({
        class_name: "status",
        child: Widget.Box({
          child: Widget.CircularProgress({
            class_name: "circular-progress",
            value: Audio.speaker.bind("volume").as((v) => v || 0),
          }),
        }),
      }),
      Widget.Icon({
        class_name: "icon",
        setup: (self) => {
          self.hook(Audio.speaker, (self) => {
            self.icon = getVolumeIcon();
          });
        },
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

export const SysTray = () =>
  Widget.Box({
    class_name: "systray",
    visible: false,
    setup: (self) => {
      self.hook(SystemTray, (self) => {
        self.visible = SystemTray.items.length > 0;
        if (!self.visible) {
          self.children = [];
        } else {
          self.children = SystemTray.items.map((item) =>
            Widget.Button({
              class_name: "systray-item",
              child: Widget.Icon({ class_name: "icon", icon: item.icon }),
              on_primary_click: (_, event) => item.activate(event),
              on_secondary_click: (_, event) => item.openMenu(event),
            }),
          );
        }
      });
    },
  });
