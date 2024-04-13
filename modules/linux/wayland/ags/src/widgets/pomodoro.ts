import { MaterialIcon } from "./icon";

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
    return "radio_button_checked";
  } else if (state === "short-break") {
    return "sync";
  } else if (state === "none") {
    return "refresh";
  } else {
    return "";
  }
};

const stateToColor = (state: string) => {
  if (state === "pomodoro") {
    return "active";
  } else if (state === "short-break") {
    return "";
  } else if (state === "none") {
    return "";
  } else {
    return "";
  }
};

export const Pomodoro = () => {
  const state = Variable("");
  const timer = Variable("?");
  const task = Variable("");
  const stateIcon = Variable("");

  return Widget.Box({
    class_name: "tasks",
    visible: false,
    children: [
      MaterialIcon(stateIcon.bind(), "state", {
        visible: stateIcon.bind().as((s) => s.length > 0),
        setup: (self) => {
          self.hook(stateIcon, (self) => {});
        },
      }),
      Widget.Label({
        class_name: "timer",
        label: timer.bind(),
        visible: state.bind().as((s) => s.length > 0 && s !== "none"),
      }),
      Widget.Label({
        class_name: "task",
        label: task.bind(),
        visible: task.bind().as((h) => h.length > 0),
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
          self.visible = true;
        } else {
          console.warn("Invalid Pomodoro result:", json);
          self.visible = false;
        }
      });
    },
  });
};
