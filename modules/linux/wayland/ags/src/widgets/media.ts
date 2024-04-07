import Gtk from "gi://Gtk";
import Gdk from "gi://Gdk";
import Mpris from "resource:///com/github/Aylur/ags/service/mpris.js";
import { withHover } from "../utils";

export const Media = () => {
  const hover = withHover();
  return Widget.EventBox({
    ...hover.attrs,
    class_name: "media",
    onPrimaryClick: () => Mpris.getPlayer("")?.playPause(),
    // onSecondaryClick: () => ,
    on_scroll_up: () => Mpris.getPlayer("")?.next(),
    on_scroll_down: () => Mpris.getPlayer("")?.previous(),
    child: Widget.Box({
      children: [
        Widget.Label({
          setup: (self) =>
            self.hook(Mpris, (self) => {
              const player = Mpris.getPlayer("");
              self.label = player ? player.identity : "";
              self.visible = player ? true : false;
            }),
        }),
        Widget.Label({
          setup: (self) =>
            self.hook(Mpris, (self) => {
              const player = Mpris.getPlayer("");
              const { track_artists, track_title } = player || {};
              const artists = track_artists ? track_artists.join(", ") : null;
              const title = player?.track_title;
              self.label = (artists ? artists + " - " : "") + (title || "");
              self.visible = player && !!title;
            }),
        }),
      ],
    }),
    setup: (self) => {
      hover.setup(self);
      self.hook(Mpris, (self) => {
        self.visible = !!Mpris.getPlayer("");
      });
    },
  });
};

export default Media;
