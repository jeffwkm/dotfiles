import { Variable } from "astal";
import { Section } from "./components";

const date = Variable("").poll(5000, "date +'%A %-m/%d/%y'");
export const Date = () => (
  <Section name="Date" icon={"calendar_today"}>
    <label label={date()} />
  </Section>
);

const time = Variable("").poll(1000, "date +'%l:%M %p'");
export const Time = () => (
  <Section name="Time" icon={"access_time"}>
    <label label={time()} />
  </Section>
);

// const date = Variable("").poll(5000, "date +'%a %b %-d'");
// const DateTime = () => (
//   <Section name="DateTime" icon={"access_time"}>
//     <label label={date()} />
//     <label label={time()} />
//   </Section>
// );

export const DateTime = () => (
  <>
    <Date />
    <Time />
  </>
);
