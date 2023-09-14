{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland.gammastep;
in {
  options.modules.wayland.gammastep = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      services.gammastep = {
        enable = true;
        provider = "manual";
        latitude = 39.02588;
        longitude = -77.15228;
        temperature.day = 6250; # 5800 6000 6200
        temperature.night = 6000; # 4800 5000 5200
      };
    };
  };
}
