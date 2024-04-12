import Gdk from "gi://Gdk";

export interface WithHover {
  attrs: {
    onHover: () => void;
    onHoverLost: () => void;
  };
  setup: (self) => void;
}

export type WithHoverFn = () => WithHover;

export const withHover: WithHoverFn = () => {
  const hover = Variable(false);
  return {
    attrs: {
      onHover: () => hover.setValue(true),
      onHoverLost: () => hover.setValue(false),
    },
    setup: (self) => {
      self.hook(hover, (self) => {
        self.window.set_cursor(
          Gdk.Cursor.new_from_name(self.get_display(), hover.getValue() ? "pointer" : "default"),
        );
      });
    },
  };
};

export const Icons = {
  SPOTIFY: "",
  CHROMIUM: "",
  CHROME: "",
  MEDIA: "",
  SHELL: "",
  EDITOR: "",
  DEFAULT: "",
};

export const playerToIcon = (identity: string | null) => {
  if (!identity) return Icons.DEFAULT;
  if (identity == "Chromium") return Icons.CHROMIUM;
  else if (identity == "Chrome") return Icons.CHROME;
  else if (identity.startsWith("mpv")) return Icons.MEDIA;
  else if (identity == "Spotify") return Icons.SPOTIFY;
  else return Icons.DEFAULT;
};

export const windowToIcon = (className: string | null) => {
  return Icons.DEFAULT;
};
