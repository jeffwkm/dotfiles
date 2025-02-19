import { Variable } from "astal";

const date = Variable("").poll(5000, "date +'%a, %h %e %Y'");
const time = Variable("").poll(1000, "date +'%l:%M %p'");

const Date = () => {
  return (
    <box className={"Date section"}>
      <label label={date()} />
    </box>
  );
};

const Time = () => {
  return (
    <box className={"Time section"}>
      <label label={time()} />
    </box>
  );
};

export { Date, Time };
