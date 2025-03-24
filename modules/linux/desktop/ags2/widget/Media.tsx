import Mpris from "gi://AstalMpris?version=0.1";
import { bind, Variable } from "astal";
import { Interactive, PlayerIcon, playerToIcon, Section } from "./components";

const mpris = Mpris.get_default();

export const selected = Variable(null as Mpris.Player | null);

const printPlayers = (players: Mpris.Player[]) => {
  // console.log(`players: ${players.length}`);
  for (const player of players) {
    // console.log(
    //   `  (${player.playback_status.toString()}) ${player.identity} ${player.artist} ${player.title}`
    // );
  }
};

const allPlayers = Variable([] as Mpris.Player[]);

const setPlayers = (players: Mpris.Player[]) => {
  const valid_players = players;
  allPlayers.set(valid_players);
  printPlayers(allPlayers.get());
  const player = selected.get();
  if (player !== null) {
    if (!valid_players.includes(player)) {
      selected.set(null);
    }
  }
};

setPlayers(mpris.players);
bind(mpris, "players").subscribe((players) => {
  setPlayers(players);
});
mpris.connect("player-added", (mpris, _player) => {
  console.log("mpris: player-added");
  printPlayers(mpris.players);
  setPlayers(mpris.players);
});
mpris.connect("player-closed", (mpris, _player) => {
  console.log("mpris: player-closed");
  printPlayers(mpris.players);
  setPlayers(mpris.players);
});

const getPlayerId = (players: Mpris.Player[], selected: Mpris.Player | null) => {
  const match = players.map((p, i) => [p, i]).find(([p]) => (p as Mpris.Player) === selected);
  if (match) return match[1] as number;
};

const nextPlayer = () => {
  const players = allPlayers.get();
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

type PlayerIconProps = {
  player: Mpris.Player | null;
};

type PlaybackStatusProps = {
  player: Mpris.Player | null;
};

const PlaybackStatus = ({ player }: PlaybackStatusProps) => {
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
      <label label={label} className={className} visible={label.as((label) => label !== "")} />
    );
  }
};

type TrackNameProps = {
  player: Mpris.Player | null;
};

const TrackName = ({ player }: TrackNameProps) => {
  if (!player) return <></>;
  const title = bind(player, "title");
  const artist = bind(player, "artist");
  return (
    <>
      {Variable.derive([title, artist], (title, artist) => {
        let label = `${artist ? artist + " - " : ""}${title ? title : ""}`;
        const maxLength = 35;
        if (label.length > maxLength) label = label.slice(0, maxLength) + "...";
        const visible = label.length > 0;
        return <label className="title" visible={visible} label={label}></label>;
      })()}
    </>
  );
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
    // console.log(`selected: ${identity} ${artist} ${title}`);
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

  if (position + delta < 0) {
    player.position = 0;
  } else if (position + delta > player.length) {
    player.position = player.length;
  } else {
    player.position = position + delta;
  }
};

export const Media = () => {
  updateSelectedPlayer(allPlayers.get());
  allPlayers.subscribe((players) => {
    updateSelectedPlayer(players);
  });

  return (
    <>
      {selected().as((player) => (
        <Interactive
          onPrimaryClick={() => playPause()}
          onSecondaryClick={() => selectNextPlayer()}
          onScrollUp={() => seekDelta(-10)}
          onScrollDown={() => seekDelta(10)}
        >
          <Section
            name={"Media"}
            icon={<PlayerIcon identity={player?.identity || ""} />}
            visible={!!player}
          >
            <TrackName player={player} />
            <PlaybackStatus player={player} />
          </Section>
        </Interactive>
      ))}
    </>
  );
};

export default Media;
