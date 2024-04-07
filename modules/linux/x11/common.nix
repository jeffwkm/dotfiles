{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.x11;
  pwd = "${host.config-dir}/modules/linux/x11";
in {
  options.modules.x11 = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    environment.sessionVariables = {
      XCURSOR_THEME = "capitaine-cursors-white";
      XCURSOR_SIZE = "24";
    };

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let inherit (config.lib.file) mkOutOfStoreSymlink;
      in {
        xdg.configFile = mkIf modules.desktop.enable {
          "gtkrc-2.0".source = mkOutOfStoreSymlink "${pwd}/gtkrc-2.0";
          "gtk-2.0/gtkfilechooser.ini".source =
            mkOutOfStoreSymlink "${pwd}/gtkfilechooser.ini";
          "gtk-3.0/".source = mkOutOfStoreSymlink "${pwd}/gtk-3.0";
          "gtk-4.0/".source = mkOutOfStoreSymlink "${pwd}/gtk-4.0";
        };
        home.file = mkIf modules.desktop.enable {
          ".Xdefaults".source = mkOutOfStoreSymlink "${pwd}/Xdefaults";
          ".Xresources.d/".source = ./Xresources.d;
          ".Xresources".source = ./Xresources.d/.Xresources.TomorrowNight;
        };
        home.packages = with pkgs; [
          mesa-demos
          xorg.xev
          xorg.xkbcomp
          xorg.xkill
          xorg.xprop
          xorg.xrandr
          xorg.xrdb
          xorg.xset
          xorg.xwininfo
          xterm
        ];
      };
  };
}
