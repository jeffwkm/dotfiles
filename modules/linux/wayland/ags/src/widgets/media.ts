import Gtk from "gi://Gtk";
import Gdk from "gi://Gdk";
import Mpris, { MprisPlayer } from "resource:///com/github/Aylur/ags/service/mpris.js";
import { withHover, playerToIcon } from "../utils";

type Mpris = typeof Mpris;

const selected = Variable(null as any);

const getPlayerId = (mpris: Mpris, selected: MprisPlayer) => {
  const match = mpris.players
    .map((p, i) => [p, i])
    .find(([p]) => (p as MprisPlayer).identity === selected.identity);
  if (match) return match[1] as number;
};

const nextPlayerId = (mpris: Mpris, selected: MprisPlayer) => {
  const id = getPlayerId(mpris, selected) || 0;
  return (id + 1) % mpris.players.length;
};

export const currentPlayer = () => {
  if (!Mpris) return null;
  if (!selected.value) {
    for (const player of Mpris.players) {
      if (player.play_back_status === "Playing") {
        selected.setValue(player);
      }
    }
  }
  if (!selected.value) selected.setValue(Mpris.getPlayer(""));
  return selected.value;
};
globalThis.currentPlayer = currentPlayer;

export const selectNextPlayer = () => {
  const id = getPlayerId(Mpris, selected.getValue());
  const player = Mpris.players[nextPlayerId(Mpris, selected.getValue())];
  selected.setValue(player);
};
globalThis.selectNextPlayer = selectNextPlayer;

const playing = () => {
  for (const player of Mpris.players) {
    if (player.play_back_status === "Playing") return player;
  }
  return null;
};

const showArtist = (player: MprisPlayer) => {
  const ignoredPlayers = ["Chromium"];
  const ignoredArtists = ["various artists", "unknown artist"];
  if (!player) return false;
  if (
    player.track_artists?.length === 1 &&
    ignoredArtists.includes(player.track_artists[0].toLowerCase())
  )
    return false;
  if (ignoredPlayers.find((x) => player.identity?.startsWith(x))) return false;
  if (player.track_artists?.join(", ").length > 40) return false;
  return true;
};

const PlayerIcon = () =>
  Widget.Label({
    class_name: "icon player",
    setup: (self) => {
      const onChange = (self) => {
        const player = currentPlayer();
        if (!selected.value) {
          self.label = "";
          self.visible = false;
        } else {
          self.label = selected ? playerToIcon(selected.value.identity) : "";
          self.visible = selected ? true : false;
        }
      };
      self.hook(selected, onChange);
      self.hook(Mpris, onChange);
    },
  });

const Status = () =>
  Widget.Label({
    setup: (self) => {
      const onChange = (self) => {
        const player = currentPlayer() as MprisPlayer;
        const status = player?.play_back_status;
        if (status === "Playing") self.label = "play_circle";
        else if (status === "Paused") self.label = "pause_circle";
        else if (status === "Stopped") self.label = "stop_circle";
        else self.label = "";
        self.class_name = `icon icon-material status ${status?.toLowerCase()}`;
        self.visible = self.label !== "";
      };
      self.hook(selected, onChange);
      self.hook(Mpris, onChange);
    },
  });

const Artist = () =>
  Widget.Label({
    class_name: "artist",
    maxWidthChars: 50,
    ellipsize: 3,
    label: "",
    visible: false,
    setup: (self) => {
      const onChange = (self) => {
        const player = currentPlayer() as MprisPlayer;
        const { track_artists } = player || {};
        const visible = track_artists && showArtist(player);
        if (visible) {
          self.label = track_artists.join(", ");
          self.visible = true;
        } else {
          self.label = "";
          self.visible = false;
        }
      };
      self.hook(selected, onChange);
      self.hook(Mpris, onChange);
    },
  });

const Title = () =>
  Widget.Label({
    class_name: "title",
    maxWidthChars: 50,
    ellipsize: 3,
    label: "",
    visible: false,
    setup: (self) => {
      const onChange = (self) => {
        const count = Mpris.players.length;
        const player = currentPlayer() as MprisPlayer;
        const title = player?.track_title;
        self.label = `${title}`;
        self.visible = !!title;
      };
      self.hook(selected, onChange);
      self.hook(Mpris, onChange);
    },
  });

export const Media = () => {
  const hover = withHover();
  let initialized = false;
  return Widget.EventBox({
    ...hover.attrs,
    class_name: "media-event-box",
    onPrimaryClick: () => currentPlayer()?.playPause(),
    onSecondaryClick: selectNextPlayer,
    on_scroll_up: () => currentPlayer()?.next(),
    on_scroll_down: () => currentPlayer()?.previous(),
    visible: Mpris.bind("players").as((ps) => ps.length > 0),
    setup: (self) => {
      hover.setup(self);
      self.hook(Mpris, (self) => {
        const player = currentPlayer();
        const active = playing() || player;
        if (!active) {
          if (initialized) self.child = Widget.Box({ visible: false });
          initialized = false;
          return;
        }
        if (active !== selected.value) selected.setValue(active);
        if (!initialized) {
          self.child = Widget.Box({
            class_name: "media",
            children: [Status(), Artist(), Title(), PlayerIcon()],
          });
          initialized = true;
        }
      });
    },
  });
};

export default Media;
