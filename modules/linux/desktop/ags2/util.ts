export const Icons = {
  SPOTIFY: "",
  CHROMIUM: "",
  CHROME: "",
  FIREFOX: "",
  MEDIA: "",
  SHELL: "",
  EDITOR: "",
  DEFAULT: "",
};

export const playerToIcon = (identity: string | null) => {
  if (!identity) return Icons.DEFAULT;
  if (identity == "Chromium") return Icons.CHROMIUM;
  else if (identity == "Chrome") return Icons.CHROME;
  else if (identity == "Firefox") return Icons.FIREFOX;
  else if (identity.startsWith("mpv")) return Icons.MEDIA;
  else if (identity.includes("Spotify")) return Icons.SPOTIFY;
  else return Icons.DEFAULT;
};
