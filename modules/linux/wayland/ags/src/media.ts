import Gtk from "gi://Gtk";
import Gdk from "gi://Gdk";

import { Mpris, Widget } from "ags-ts";
const { Button, Label, EventBox, Box } = Widget;
import { watch, merge } from "resource:///com/github/Aylur/ags/utils.js";

export const Media = () => {
  // const watchPrint = watch(null, Mpris, "changed", () => {
  //   console.log(Mpris.players);
  // });

  const hover = Variable(false);

  return EventBox({
    onHover: () => hover.setValue(true),
    onHoverLost: () => hover.setValue(false),
    class_name: "media",
    onPrimaryClick: () => Mpris.getPlayer("")?.playPause(),
    // onSecondaryClick: () => ,
    on_scroll_up: () => Mpris.getPlayer("")?.next(),
    on_scroll_down: () => Mpris.getPlayer("")?.previous(),
    child: Box({
      children: [
        Label({
          setup: (self) =>
            self.hook(Mpris, (self1) => {
              const player = Mpris.getPlayer("");
              self1.label = player ? player.identity : undefined;
              self1.visible = player ? true : false;
            }),
        }),
        Label({
          setup: (self) =>
            self.hook(Mpris, (self1) => {
              const player = Mpris.getPlayer("");
              const { track_artists, track_title } = player || {};
              const artists = track_artists ? track_artists.join(", ") : null;
              const title = player?.track_title || null;
              self1.label = (artists ? artists + " - " : "") + (title || "");
              self1.visible = player && title ? true : false;
            }),
        }),
      ],
    }),
    setup: (self) => {
      watch(null, hover, "changed", () =>
        self.window.set_cursor(
          Gdk.Cursor.new_from_name(self.get_display(), hover.getValue() ? "pointer" : "default"),
        ),
      );
      return self.hook(Mpris, (self1) => {
        self1.visible = !!Mpris.getPlayer("");
      });
    },
  });
};
