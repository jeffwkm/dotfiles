{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user;
  cfg = config.modules.services.mpd;
in {
  options.modules.services.mpd = with types; {
    enable = mkBoolOpt false;
    musicDirectory = mkOpt (nullOr str) null;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ mpd mpd-mpris mpc-cli ncmpcpp ];

    hardware.pulseaudio.extraConfig =
      "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";

    services.mpd = {
      enable = true;
      musicDirectory = cfg.musicDirectory;
      extraConfig = ''
        audio_output {
          type "pulse"
          name "Pulseaudio"
          server "127.0.0.1" # add this line - MPD must connect to the local sound server
        }
      '';
      network.listenAddress = "any";
      startWhenNeeded = true;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
