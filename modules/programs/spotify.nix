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
        [ ncspot spotify-tui ] ++ optional cfg.spotifyd.enable [ spotifyd ]
        ++ optionals modules.desktop.enable [
          spotify
          spotifywm # :: Wrapper around Spotify that correctly sets class name before opening the window
        ];

      services.spotifyd = mkIf cfg.spotifyd.enable { enable = true; };
    };
  };
}
