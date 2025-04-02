import { Variable, bind, exec } from "astal";
import GLib from "gi://GLib?version=2.0";
import { Command, Section } from "./components";

enum PomodoroState {
  Pomodoro = "pomodoro",
  ShortBreak = "short-break",
  LongBreak = "long-break",
  None = "none",
}

interface PomodoroResult {
  state: string;
  timer: string;
  seconds: number;
  heading: string;
}

const stateToIcon = (state: PomodoroState): string => {
  switch (state) {
    case PomodoroState.Pomodoro:
      return "schedule";
    case PomodoroState.ShortBreak:
      return "refresh";
    case PomodoroState.LongBreak:
      return "check_circle";
    case PomodoroState.None:
      return "schedule";
  }
};

const stateToClass = (state: PomodoroState): string => {
  switch (state) {
    case PomodoroState.Pomodoro:
      return "active";
    case PomodoroState.ShortBreak:
      return "short-break";
    case PomodoroState.LongBreak:
      return "long-break";
    case PomodoroState.None:
      return "waiting";
    default:
      return "";
  }
};

export const pomodoroResult = Variable("");

// Set up polling
GLib.timeout_add(GLib.PRIORITY_DEFAULT, 1000, () => {
  const cmd = ["emacsclient", "-e", "(--org-pomodoro-status-json)"];
  try {
    pomodoroResult.set(exec(cmd.join(" ")));
    return GLib.SOURCE_CONTINUE;
  } catch (error) {
    console.warn("Failed to execute emacsclient:", error);
    return GLib.SOURCE_REMOVE;
  }
});

type TaskLabelProps = {
  text: Variable<string>;
};

const TaskLabel = (props: TaskLabelProps) => {
  const { text } = props;

  return <label className="TaskLabel" label={text()} visible={text().as((h) => h.length > 0)} />;
};

export const Pomodoro = () => {
  const state = Variable(PomodoroState.None);
  const timer = Variable("?");
  const task = Variable("");
  const stateIcon = Variable("");
  const stateClass = Variable("");
  const text = Variable("");
  const visible = Variable(false);

  pomodoroResult.subscribe(() => {
    try {
      const json = JSON.parse(pomodoroResult.get());
      const result = JSON.parse(json) as PomodoroResult;

      if (result) {
        timer.set(result.timer);

        if (result.state === PomodoroState.Pomodoro) {
          task.set(result.heading);
        } else {
          task.set("");
        }

        if (
          result.state === PomodoroState.Pomodoro ||
          result.state === PomodoroState.ShortBreak ||
          result.state === PomodoroState.LongBreak
        ) {
          state.set(result.state);
        } else {
          state.set(PomodoroState.None);
        }

        stateIcon.set(stateToIcon(state.get()));
        stateClass.set(stateToClass(state.get()));

        text.set(
          result.state === PomodoroState.Pomodoro
            ? task.get()
            : result.state === PomodoroState.ShortBreak
            ? "* Short break! *"
            : result.state === PomodoroState.LongBreak
            ? "* Long break! *"
            : "SPC n r d t ..."
        );

        visible.set(true);
      } else {
        console.warn("Invalid Pomodoro result:", json);
        visible.set(false);
      }
    } catch (error) {
      console.warn("Failed to parse Pomodoro result:", error);
      visible.set(false);
    }
  });

  const boundIcon = bind(stateIcon);

  return (
    <>
      {boundIcon.as((icon: string) => (
        <>
          <Section
            name="Pomodoro"
            icon={<Command label="Tasks" command={"S-,"} />}
            className={stateClass()}
            visible={visible()}
          >
            {state().as((s) => {
              if (s === PomodoroState.None) {
                return (
                  <>
                    <Command label="Find" command={"SPC n r f"} />
                    <Command label="Today" command={"SPC n r d t"} />
                  </>
                );
              } else if (s === PomodoroState.Pomodoro) {
                return (
                  <>
                    <label className="TaskLabel" label={task()} />

                    <box className="monospace Timer Command">
                      <label
                        label={timer()}
                        visible={state().as((s) => s === PomodoroState.Pomodoro)}
                      />
                    </box>
                  </>
                );
              } else {
                return null;
              }
            })}
          </Section>
        </>
      ))}
    </>
  );
};
