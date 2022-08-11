{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host;
  cfg = config.modules.services.ydotool;
in {
  options.modules.services.ydotool = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ ydotool ];

      # TODO: this needs to run as root
      systemd.user.services.ydotool = {
        Unit = {
          Description = "Starts ydotoold service";
          PartOf = [ "graphical-session.target" ];
          After = [ "sway.target" ];
        };
        Install = { WantedBy = [ "sway.target" ]; };
        Service = {
          Type = "simple";
          Restart = "always";
          ExecStart = "${pkgs.ydotool}/bin/ydotoold";
          ExecReload = "${pkgs.util-linux}/bin/kill -HUP $MAINPID";
          KillMode = "process";
          TimeoutSec = 180;
        };
      };
    };
  };
}
