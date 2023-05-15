{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.programs.spotify;
in {
  options.modules.programs.spotify = {
    enable = mkBoolOpt false;
    spotifyd.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = with pkgs;
        [ ncspot spotify-tui ] ++ optionals modules.desktop.enable [ spotify ];

      services.spotifyd = mkIf cfg.spotifyd.enable {
        enable = true;
        package = pkgs.spotifyd.override {
          withPulseAudio = true;
          withMpris = true;
        };
        settings = {
          global = {
            username = "${user.email}";
            password_cmd = "pass show media/spotify";
            device_name = "${host.name}";
          };
        };
      };
    };
  };
}
