{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.services.protonmail;
  gui = modules.desktop.enable;
in {
  options.modules.services.protonmail = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ protonmail-bridge ] ++ (optionals gui [ protonmail-bridge-gui ]);

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      systemd.user.services = {
        protonmail-bridge = {
          Unit = {
            Description = "ProtonMail Bridge";
            After = [ "network.target" "graphical-session.target" ];
          };
          Service = {
            Type = "simple";
            ExecStart =
              "${pkgs.protonmail-bridge}/bin/protonmail-bridge --grpc";
          };
          Install = { WantedBy = [ "graphical-session.target" ]; };
        };
      };
    };
  };
}
