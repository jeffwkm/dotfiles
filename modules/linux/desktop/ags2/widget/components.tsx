import { bind, Binding } from "astal";
import { Gdk } from "astal/gtk3";
import { Astal } from "astal/gtk3";
import { BoxProps, CircularProgress, EventBox, LabelProps } from "astal/gtk3/widget";

type MaterialIconProps = {
  icon: string;
} & LabelProps;

export const MaterialIcon = ({ icon, className = "", ...props }: MaterialIconProps) => {
  return <label label={icon} className={`icon icon-material ${className}`} {...props} />;
};

export const Icons = {
  SPOTIFY: "",
  CHROMIUM: "",
  CHROME: "",
  FIREFOX: "",
  MEDIA: "",
  SHELL: "",
  EDITOR: "",
  DEFAULT: "",
};

export const playerToIcon = (identity: string | null) => {
  if (!identity) return Icons.DEFAULT;
  const lower = identity.toLowerCase();
  if (lower.includes("chromium")) return "web"; // return Icons.CHROMIUM;
  else if (lower.includes("chrome")) return "web"; // return Icons.CHROME;
  else if (lower.includes("firefox")) return "web"; // return Icons.FIREFOX;
  else if (lower.startsWith("mpv")) return Icons.MEDIA;
  else if (lower.includes("spotify")) return Icons.SPOTIFY;
  else return Icons.DEFAULT;
};

export const PlayerIcon = ({ identity }: { identity: string }) => {
  const icon = playerToIcon(identity);
  if (icon.length === 1) {
    return <label label={icon} className={"icon PlayerIcon"} />;
  } else {
    return <MaterialIcon className={"PlayerIcon"} icon={icon} />;
  }
};

type PercentProgressProps = {
  value: Binding<number>;
} & BoxProps;

export const PercentProgress = ({ value, className = "", ...props }: PercentProgressProps) => (
  <>
    {bind(value).as((value) => {
      const percent = Math.round(value * 100);
      let label = percent.toString();
      if (percent == 100) label = "+";
      if (percent > 100) label = "++";
      return (
        <box className={`ProgressBox ${className}`} {...props}>
          <CircularProgress value={value} />
          <label label={label} />
        </box>
      );
    })}
  </>
);

type SectionProps = {
  icon?: JSX.Element | string;
  name: string;
  child?: JSX.Element;
  children?: Array<JSX.Element>;
} & BoxProps;

export const Section = ({
  icon,
  name,
  className = "",
  child,
  children,
  ...props
}: SectionProps) => (
  <box className={`${name} ${className} section`} {...props}>
    {icon && typeof icon === "string" ? (
      icon.length === 1 ? (
        <label label={icon} className={"icon"} />
      ) : (
        <MaterialIcon icon={icon} />
      )
    ) : (
      icon
    )}
    {child ? child : children ? children : null}
  </box>
);

type InteractiveProps = {
  onPrimaryClick?: () => void;
  onSecondaryClick?: () => void;
  onScrollUp?: () => void;
  onScrollDown?: () => void;
  child?: JSX.Element;
  children?: Array<JSX.Element>;
};

export const Interactive = ({
  onPrimaryClick,
  onSecondaryClick,
  onScrollUp,
  onScrollDown,
  child,
  children,
}: InteractiveProps) => {
  return (
    <EventBox
      onClickRelease={(_self: EventBox, event: Astal.ClickEvent) => {
        if (event.button === Gdk.BUTTON_PRIMARY) onPrimaryClick?.();
        else if (event.button === Gdk.BUTTON_SECONDARY) onSecondaryClick?.();
      }}
      onScroll={(_self: EventBox, event: Astal.ScrollEvent) => {
        if (event.delta_y < 0) onScrollUp?.();
        else if (event.delta_y > 0) onScrollDown?.();
      }}
      cursor="pointer"
    >
      {child ? child : children ? children : null}
    </EventBox>
  );
};

type CommandProps = {
  command: string;
  label?: string;
} & BoxProps;

export const Command = ({ command, label, ...props }: CommandProps) => (
  <box className="Command" {...props}>
    <label className="CommandLabel" label={label || ""} visible={!!label} />
    <label label={command} />
  </box>
);
