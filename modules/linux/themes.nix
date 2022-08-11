{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  enable = (modules.desktop.enable);
in {
  config = mkIf enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs;
        [ vivid base16-builder base16-shell-preview ] ++ [
          arc-icon-theme
          arc-theme
          beauty-line-icon-theme
          capitaine-cursors
          gnome.adwaita-icon-theme
          gnome-icon-theme
          humanity-icon-theme
          materia-theme
          nordic
          numix-cursor-theme
          numix-gtk-theme
          numix-icon-theme
          openzone-cursors
          pantheon.elementary-icon-theme
          paper-gtk-theme
          paper-icon-theme
          papirus-icon-theme
          skeu
          theme-vertex
          ubuntu-themes
          venta
        ] ++ optionals modules.desktop.qt.enable [
          plasma5Packages.qtstyleplugin-kvantum
          adwaita-qt
        ];
    };
  };
}
