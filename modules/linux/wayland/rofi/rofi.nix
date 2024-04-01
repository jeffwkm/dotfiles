{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland.rofi;
in {
  options.modules.wayland.rofi = { enable = mkBoolOpt modules.wayland.enable; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ (wrapOptimize config "rofi-wayland-unwrapped") ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      xdg.configFile."rofi/config.rasi".source =
        config.lib.file.mkOutOfStoreSymlink
        "${host.config-dir}/modules/linux/wayland/rofi/config.rasi";
      home.file.".local/share/rofi/themes".source =
        config.lib.file.mkOutOfStoreSymlink
        "${host.config-dir}/modules/linux/wayland/rofi/themes";

      home.packages = with pkgs; [
        (rofi-wayland.override { plugins = [ rofi-top ]; })
        rofi-pass-wayland
        (rofi-pulse-select.override {
          rofi-unwrapped = rofi-wayland-unwrapped;
        })
        (rofi-systemd.override { rofi = rofi-wayland; })
      ];
    };
  };
}
