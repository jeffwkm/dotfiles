export interface PomodoroResult {
  state: string;
  timer: string;
  seconds: number;
  heading: string;
}

export const pomodoroResult = Variable("", {
  poll: [1000, "emacsclient -e '(--org-pomodoro-status-json)'"],
});

const stateToIcon = (state: string) => {
  if (state === "pomodoro") {
    return "schedule";
    // return "hourglass_empty";
  } else if (state === "short-break") {
    return "refresh";
  } else if (state === "long-break") {
    return "check_circle";
  } else if (state === "none") {
    return "schedule";
  } else {
    return "";
  }
};

const stateToClass = (state: string) => {
  if (state === "pomodoro") {
    return "active";
  } else if (state === "short-break") {
    return "short-break";
  } else if (state === "long-break") {
    return "long-break";
  } else if (state === "none") {
    return "waiting";
  } else {
    return "";
  }
};

export const Pomodoro = () => {
  const state = Variable("");
  const timer = Variable("?");
  const task = Variable("");
  const stateIcon = Variable("");
  const stateClass = Variable("");
  const text = Variable("");

  return Widget.Box({
    class_name: stateClass.bind().as((c) => `tasks ${c}`),
    visible: false,
    children: [
      Widget.Box({
        class_name: "state-with-timer",
        children: [
          Widget.Label({
            class_name: "icon icon-material state",
            label: stateIcon.bind(),
            visible: stateIcon.bind().as((s) => s.length > 0),
          }),
          Widget.Label({
            class_name: "timer",
            label: timer.bind(),
            visible: state.bind().as((s) => s.length > 0 && s !== "none"),
          }),
        ],
      }),
      Widget.Label({
        class_name: "task",
        label: text.bind(),
        visible: text.bind().as((h) => h.length > 0),
      }),
    ],
    setup: (self) => {
      self.hook(pomodoroResult, (self) => {
        const json = JSON.parse(pomodoroResult.value);
        const result = JSON.parse(json) as PomodoroResult;
        if (result) {
          timer.value = result.timer;
          task.value = result.heading;
          state.value = result.state;
          stateIcon.value = stateToIcon(result.state);
          stateClass.value = stateToClass(result.state);
          if (result.state === "pomodoro") {
            text.value = task.value;
          } else if (result.state === "short-break") {
            text.value = "* Short break! *";
          } else if (result.state === "long-break") {
            text.value = "* Long break! *";
          } else {
            text.value = "Waiting for a task...";
          }
          self.visible = true;
        } else {
          console.warn("Invalid Pomodoro result:", json);
          self.visible = false;
        }
      });
    },
  });
};
