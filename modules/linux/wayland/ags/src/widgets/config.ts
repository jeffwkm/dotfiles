export const wallpapersPath = "~/.config/hypr/wallpapers";

export const wallpapers = ["wall0.jpg", "wall1.jpg", "wall2.jpg"];

export interface Group {
  wallpaper?: string;
}
export type Groups = Group[];

export const groups: Groups = [
  {
    wallpaper: "wall2.jpg",
  },
  {
    wallpaper: "wall0.jpg",
  },
];

export interface Config {
  defaultWallpaper: string;
}

export const config: Config = {
  defaultWallpaper: "wall0.jpg",
};
