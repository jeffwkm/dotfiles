import { Variable } from "astal";
import { MaterialIcon, Section } from "./components";

// const date = Variable("").poll(5000, "date +'%a, %h %e'");

const date = Variable("").poll(5000, "date +'%A %-m/%d/%y'");

const time = Variable("").poll(1000, "date +'%l:%M %p'");

const Date = () => (
  <Section name="Date" icon={"calendar_today"}>
    <label label={date()} />
  </Section>
);

const Time = () => (
  <Section name="Time" icon={"access_time"}>
    <label label={time()} />
  </Section>
);

export { Date, Time };
