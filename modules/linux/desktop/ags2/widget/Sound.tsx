import Wireplumber from "gi://AstalWp?version=0.1";
import { bind } from "astal";
import { Interactive, PercentProgress, Section } from "./components";

const wp = Wireplumber.get_default();

type VolumeProps = {
  speaker: Wireplumber.Endpoint;
};

const Volume = ({ speaker }: VolumeProps) => {
  if (!wp) return null;

  const volume = bind(speaker, "volume");
  const volume_icon = bind(speaker, "volume_icon");

  return (
    <Interactive
      onPrimaryClick={() => (speaker.mute = !speaker.mute)}
      onScrollUp={() => (speaker.volume += 0.05)}
      onScrollDown={() => (speaker.volume -= 0.05)}
    >
      <Section name={"Volume"} icon={<icon icon={volume_icon} />}>
        <PercentProgress value={volume} className={"VolumeProgress"} />
      </Section>
    </Interactive>
  );
};

const Sound = () => {
  if (!wp) return null;
  const default_speaker = bind(wp, "default_speaker");
  return (
    <>
      {default_speaker.as((speaker) => (
        <Volume speaker={speaker} />
      ))}
    </>
  );
};

export default Sound;
