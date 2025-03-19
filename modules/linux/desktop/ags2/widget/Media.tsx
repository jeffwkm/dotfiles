import Gdk from "gi://Gdk";
import Mpris from "gi://AstalMpris?version=0.1";
import { bind, Variable } from "astal";
import { playerToIcon } from "../util";
import { Box, EventBox, Label } from "astal/gtk3/widget";
import Pango from "gi://Pango?version=1.0";

const mpris = Mpris.get_default();

export const selected = Variable(null as Mpris.Player | null);

const printPlayers = (players: Mpris.Player[]) => {
  console.log(`players: ${players.length}`);
  for (const player of players) {
    console.log(
      `  (${player.playback_status.toString()}) ${player.identity} ${player.artist} ${player.title}`
    );
  }
};

const validPlayers = Variable([] as Mpris.Player[]);

const validPlayer = (p: Mpris.Player) =>
  p.playback_status !== Mpris.PlaybackStatus.STOPPED && p.title.length > 0;

const setValidPlayers = (players: Mpris.Player[]) => {
  const valid_players = players.filter(validPlayer);
  validPlayers.set(valid_players);
  // validPlayers.set(players);
  printPlayers(validPlayers.get());
  const player = selected.get();
  if (player !== null) {
    if (!valid_players.includes(player)) {
      selected.set(null);
    }
  }
};

setValidPlayers(mpris.players);

// mpris.connect("player-added", (_mpris, player) => {
//   const players = validPlayers.get();
//   if (validPlayer(player) && !players.includes(player)) {
//     console.log(`player-added: ${player.title}`);
//     validPlayers.set([...players, player]);
//   }
// });

// mpris.connect("player-closed", (_mpris, player) => {
//   const players = validPlayers.get();
//   if (players.includes(player)) {
//     console.log(`player-closed: ${player.title}`);
//     validPlayers.set(players.filter((p) => p !== player));
//   }
// });

bind(mpris, "players").subscribe((players) => {
  setValidPlayers(players);
});

const getPlayerId = (players: Mpris.Player[], selected: Mpris.Player | null) => {
  const match = players.map((p, i) => [p, i]).find(([p]) => (p as Mpris.Player) === selected);
  if (match) return match[1] as number;
};

const nextPlayer = () => {
  const players = validPlayers.get();
  const current = selected.get();
  const id = getPlayerId(players, current) || 0;
  console.log(`current: ${current?.identity}`);
  console.log(`id: ${id}`);
  console.log(`players: ${players.length}`);
  const nextId = (id + 1) % players.length;
  console.log(`nextId: ${nextId}`);
  return players[nextId];
};

export const selectNextPlayer = () => {
  selected.set(nextPlayer());
};

export const currentPlayer = (selected_player: Mpris.Player | null, players: Mpris.Player[]) => {
  let result = null;
  if (!mpris || players.length === 0) {
    result = null;
  } else if (selected_player) {
    result = selected_player;
  } else if (players.length > 0) {
    for (const p of players) {
      if (p.playback_status === Mpris.PlaybackStatus.PLAYING) result = p;
    }
    if (!result) result = players[0];
  }
  return result;
};

const playing = (players: Mpris.Player[]) => {
  for (const p of players) {
    if (p.playback_status === Mpris.PlaybackStatus.PLAYING) return p;
  }
  return null;
};

const showArtist = (player: Mpris.Player) => {
  const ignoredPlayers = ["Chromium", "Firefox"];
  const ignoredArtists = ["various artists", "unknown artist"];
  if (!player) return false;
  if (ignoredArtists.includes(player.artist?.toLowerCase())) return false;
  if (ignoredPlayers.find((x) => player.identity?.startsWith(x))) return false;
  if (player.artist?.length > 40) return false;
  return true;
};

type PlayerIconProps = {
  player: Mpris.Player | null;
};

const PlayerIcon = ({ player }: PlayerIconProps) => (
  <Label
    className={"PlayerIcon icon icon-material"}
    label={player ? playerToIcon(player.identity) : ""}
    visible={!!player}
  />
);

type StatusProps = {
  player: Mpris.Player | null;
};

const PlaybackStatus = ({ player }: StatusProps) => {
  if (!player) return <></>;
  else {
    const status = bind(player, "playback_status");
    const label = status.as((status) => {
      let label = "";
      if (status === Mpris.PlaybackStatus.PLAYING) label = "play_circle";
      else if (status === Mpris.PlaybackStatus.PAUSED) label = "pause_circle";
      else if (status === Mpris.PlaybackStatus.STOPPED) label = "stop_circle";
      return label;
    });
    const className = status.as((status) => {
      return `PlaybackStatus ${statusDescription(status)} icon icon-material `;
    });
    return (
      <Label label={label} className={className} visible={label.as((label) => label !== "")} />
    );
  }
};

type ArtistProps = {
  player: Mpris.Player | null;
};

const Artist = ({ player }: ArtistProps) => {
  const artist = player?.artist;
  const visible = !!(artist && showArtist(player));
  return (
    <Label
      className="artist"
      maxWidthChars={20}
      // ellipsize={Pango.EllipsizeMode.END}
      visible={visible}
      label={artist ? artist : ""}
    />
  );
};

type TitleProps = {
  player: Mpris.Player | null;
};

const Title = ({ player }: TitleProps) => {
  if (!player) return <></>;
  else {
    return (
      <>
        {bind(player, "title").as((title) => {
          const visible = !!title;
          return (
            <Label
              className="title"
              // maxWidthChars={}
              // widthChars={title ? title.length : 0}
              // ellipsize={Pango.EllipsizeMode.END}
              visible={visible}
              label={title ? title : ""}
            />
          );
        })}
      </>
    );
  }
};

const updateSelectedPlayer = (players: Mpris.Player[]) => {
  selected.set(currentPlayer(selected.get(), players));
  if (selected().get() === null) {
    const firstPlaying = playing(players);
    if (firstPlaying) selected.set(firstPlaying);
  }
  const value = selected.get();
  if (value !== null) {
    const identity = value.identity;
    const artist = value.artist;
    const title = value.title;
    console.log(`selected: ${identity} ${artist} ${title}`);
  }
};

const statusDescription = (status: Mpris.PlaybackStatus | null) => {
  if (status === Mpris.PlaybackStatus.PLAYING) return "playing";
  else if (status === Mpris.PlaybackStatus.PAUSED) return "paused";
  else if (status === Mpris.PlaybackStatus.STOPPED) return "stopped";
  else return "unknown status";
};

export const playPause = () => {
  selected.get()?.play_pause();
};

export const nextTrack = () => {
  selected.get()?.next();
};

export const previousTrack = () => {
  selected.get()?.previous();
};

export const seekDelta = (delta: number) => {
  const player = selected.get();
  if (!player) return;
  const position = player?.position;
  if (!position) return;
  player.position = position + delta;
};

export const Media = () => {
  updateSelectedPlayer(validPlayers.get());
  validPlayers.subscribe((players) => {
    updateSelectedPlayer(players);
  });

  return (
    <>
      {selected().as((player) => {
        return (
          <EventBox
            className={"MediaEventBox"}
            cursor="pointer"
            onClick={(_self, e) => {
              if (e.button === Gdk.BUTTON_PRIMARY) playPause();
              else if (e.button === Gdk.BUTTON_SECONDARY) selectNextPlayer();
            }}
            onScroll={(_self, e) => {
              if (e.delta_y > 0) seekDelta(10);
              else if (e.delta_y < 0) seekDelta(-10);
            }}
            visible={!!player}
          >
            <Box className={`Media section`} visible={!!player}>
              <PlayerIcon player={player} />
              <Artist player={player} />
              <Title player={player} />
              <PlaybackStatus player={player} />
            </Box>
          </EventBox>
        );
      })}
    </>
  );
};

export default Media;
