{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    mpv
    gnome-mpv
    mpvc
    play-with-mpv
  ];
}
