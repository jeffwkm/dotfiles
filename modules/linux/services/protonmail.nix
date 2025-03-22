{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = modules.services.protonmail;
  gui = modules.desktop.enable;
  pwd = "${host.config-dir}/modules/linux/services";
in {
  options.modules.services.protonmail = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ protonmail-bridge mu isync ]
      ++ (optionals gui [ protonmail-bridge-gui ]);

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.file.".mbsyncrc".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/mbsyncrc";

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

        mbsync = {
          Unit = {
            Description = "Local mailbox synchronization service";
            After = [
              "network.target"
              "graphical-session.target"
              "protonmail-bridge.service"
            ];
            Requires = [ "protonmail-bridge.service" ];
          };
          Service = let
            mbsync-loop = pkgs.writeShellScript "mbsync-loop" ''
              while true ; do mbsync -a ; sleep 900 ; done
            '';
          in {
            Type = "simple";
            ExecStart = "${mbsync-loop}";
            Restart = "always";
            RestartSec = 1800;
          };
          Install = { WantedBy = [ "graphical-session.target" ]; };
        };

        mu-init = {
          Unit = {
            Description = "Local mailbox index service";
            After = [ "mbsync.service" ];
          };
          Service = let
            mu-init = pkgs.writeShellScript "mu-init" ''
              [ -e "$HOME/.cache/mu" ] && exit 0
              mu init --maildir=~/.mail/protonmail/ --my-address='/jeff\.workman.*/' --my-address='/asdf...@protonmail\.com/'
              mu index
            '';
          in {
            Type = "oneshot";
            ExecStart = "${mu-init}";
          };
        };
      };
    };
  };
}
