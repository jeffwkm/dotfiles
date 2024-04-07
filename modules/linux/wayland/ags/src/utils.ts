import Gdk from "gi://Gdk";

export interface WithHover<W> {
  attrs: {
    onHover: () => void;
    onHoverLost: () => void;
  };
  setup: (self: W) => void;
}

export type WithHoverFn<W> = () => WithHover<W>;

export const withHover: WithHoverFn<W> = () => {
  const hover = Variable(false);
  return {
    attrs: {
      onHover: () => hover.setValue(true),
      onHoverLost: () => hover.setValue(false),
    },
    setup: (self: W) => {
      self.hook(hover, (self) => {
        self.window.set_cursor(
          Gdk.Cursor.new_from_name(self.get_display(), hover.getValue() ? "pointer" : "default"),
        );
      });
    },
  };
};
