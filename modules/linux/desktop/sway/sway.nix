{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  pwd = "${host.config-dir}/modules/linux/desktop/sway";
  cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ swaybg swayidle swaylock swaytools ];

      xdg.configFile."sway/config" = {
        source = config.lib.file.mkOutOfStoreSymlink "${pwd}/config";

        # NOTE: onChange doesn't work with mkOutOfStoreSymlink
        onChange = "${pkgs.writeShellScript "sway-change" ''
          set +e
          socket=$(ls /run/user/1000/sway*)
          swaymsg -s "$socket" -- reload || true
        ''}";
      };

      systemd.user.targets.sway-session = {
        Unit = {
          Description = "Sway compositor session";
          BindsTo = [ "graphical-session.target" ];
        };
      };

      # systemd.user.services.mako.Install.WantedBy =
      #   mkIf modules.desktop.mako.enable [ "sway-session.target" ];

      systemd.user.services.waybar.Install.WantedBy =
        mkIf modules.desktop.waybar.enable [ "sway-session.target" ];

      systemd.user.services.swayidle = {
        Unit = {
          Description = "Idle manager for Sway compositor";
          PartOf = [ "graphical-session.target" ];
        };
        Install = { WantedBy = [ "sway-session.target" ]; };
        Service = {
          Type = "simple";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          ExecStart = ''
            ${pkgs.swayidle}/bin/swayidle -w \
                  timeout 1800   'swaylock -f -c 000000' \
                  timeout 3600   'swaymsg "output * dpms off"' \
                  resume         'swaymsg "output * dpms on"' \
                  before-sleep   'swaylock -f -c 000000' \
          '';
          Restart = "always";
          RestartSec = 60;
        };
      };
    };
  };
}
