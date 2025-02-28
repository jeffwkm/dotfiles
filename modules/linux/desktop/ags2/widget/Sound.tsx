import { bind, Variable } from "astal";
import Wireplumber from "gi://AstalWp?version=0.1";
import { Widget } from "astal/gtk3";
const { CircularProgress } = Widget;

const wp = Wireplumber.get_default();

type VolumeProps = {
  speaker: Wireplumber.Endpoint;
};

const Volume = ({ speaker }: VolumeProps) => {
  if (!wp) return null;

  const volume = bind(speaker, "volume");
  const volume_icon = bind(speaker, "volume_icon");
  const volume_label = Variable("");
  volume.subscribe((volume) => {
    const percent = Math.round(volume * 100);
    volume_label.set(`${percent}%`);
  });

  return (
    <box className={"Volume section"}>
      <icon icon={volume_icon.as((icon) => icon)} />
      <box className={"VolumeProgress"}>
        <CircularProgress value={volume} heightRequest={8} />
      </box>
    </box>
  );
};

const Sound = () => {
  if (!wp) return null;
  const default_speaker = bind(wp, "default_speaker");
  return (
    <>
      {default_speaker.as((speaker) => {
        return <Volume speaker={speaker} />;
      })}
    </>
  );
};

export default Sound;
