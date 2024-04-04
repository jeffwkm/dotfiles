import { Mpris, Widget } from "ags-ts";
const { Button, Label } = Widget;
import { watch } from "resource:///com/github/Aylur/ags/utils.js";

export const Media = () => {
  const label = watch("", Mpris, "player-changed", () => {
    if (Mpris.players[0]) {
      const { track_artists, track_title } = Mpris.players[0];
      return `${track_artists.join(", ")} - ${track_title}`;
    } else {
      return "Nothing is playing";
    }
  });

  return Button({
    class_name: "media",
    on_primary_click: () => Mpris.getPlayer("")?.playPause(),
    on_scroll_up: () => Mpris.getPlayer("")?.next(),
    on_scroll_down: () => Mpris.getPlayer("")?.previous(),
    child: Label({ label }),
  });
};
