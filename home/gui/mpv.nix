{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [ mpv celluloid mpvc play-with-mpv ];
}
