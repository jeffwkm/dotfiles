import Gtk from "gi://Gtk";
import Gdk from "gi://Gdk";
import Mpris, { MprisPlayer } from "resource:///com/github/Aylur/ags/service/mpris.js";
import { withHover, playerToIcon } from "../utils";
import { watch } from "resource:///com/github/Aylur/ags/utils.js";

type Mpris = typeof Mpris;

const selected = Variable(null as any);

const allPlayers = Variable([] as MprisPlayer[]);

const validPlayer = (p: MprisPlayer) =>
  p.play_back_status !== "Stopped" && p.track_title.length > 0;

watch(null, Mpris, () => {
  allPlayers.setValue(Mpris.players.filter(validPlayer));
});

const getPlayerId = (selected: MprisPlayer) => {
  const match = allPlayers.value
    .map((p, i) => [p, i])
    .find(([p]) => (p as MprisPlayer).identity === selected.identity);
  if (match) return match[1] as number;
};

const nextPlayer = (selected: MprisPlayer) => {
  const id = getPlayerId(selected) || 0;
  const nextId = (id + 1) % allPlayers.value.length;
  return allPlayers.value[nextId];
};

export const currentPlayer = () => {
  if (!Mpris || allPlayers.value.length === 0) {
    selected.setValue(null);
    return null;
  }
  if (selected.value) return selected.value;
  for (const p of allPlayers.value) {
    if (p.play_back_status === "Playing") selected.setValue(p);
  }
  if (!selected.value) selected.setValue(allPlayers.value[0]);
  return selected.value;
};
globalThis.currentPlayer = currentPlayer;

export const selectNextPlayer = () => {
  const id = getPlayerId(selected.value);
  const player = nextPlayer(selected.value);
  selected.setValue(player);
};
globalThis.selectNextPlayer = selectNextPlayer;

const playing = () => {
  for (const p of allPlayers.value) {
    if (p.play_back_status === "Playing") return p;
  }
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
          self.visible = !!selected;
        }
      };
      self.hook(Mpris, onChange);
      self.hook(selected, onChange);
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
      self.hook(Mpris, onChange);
      self.hook(selected, onChange);
    },
  });

const Artist = () =>
  Widget.Label({
    class_name: "artist",
    maxWidthChars: 50,
    ellipsize: 3,
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
      self.hook(Mpris, onChange);
      self.hook(selected, onChange);
    },
  });

const Title = () =>
  Widget.Label({
    class_name: "title",
    maxWidthChars: 50,
    ellipsize: 3,
    setup: (self) => {
      const onChange = (self) => {
        const count = Mpris.players.length;
        const player = currentPlayer() as MprisPlayer;
        const title = player?.track_title;
        self.label = `${title}`;
        self.visible = !!title;
      };
      self.hook(Mpris, onChange);
      self.hook(selected, onChange);
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
    setup: (self) => {
      hover.setup(self);
      const doUpdate = (self) => {
        const player = currentPlayer();
        if (!player) {
          if (initialized) self.child = Widget.Box({ visible: false });
          self.visible = false;
          initialized = false;
          return;
        }
        if (player.play_back_status !== "Playing") {
          const firstPlaying = playing();
          if (firstPlaying) selected.setValue(firstPlaying);
        }
        if (!initialized) {
          self.child = Widget.Box({
            class_name: "media",
            children: [Status(), Artist(), Title(), PlayerIcon()],
          });
          self.visible = true;
          initialized = true;
        }
      };
      self.hook(Mpris, doUpdate);
      self.hook(allPlayers, doUpdate);
    },
  });
};

export default Media;
