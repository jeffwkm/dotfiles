{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config) user host;
  cfg = config.modules.wayland.mako;
  pwd = "${host.config-dir}/modules/linux/wayland";
in {
  options.modules.wayland.mako = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = [ pkgs.mako ];

      xdg.configFile."mako/config".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/_mako.config";

      systemd.user.services.mako = {
        Unit = {
          Description = "Mako notification daemon";
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Notifications";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          ExecStart = "${pkgs.mako}/bin/mako";
          ExecReload = "${pkgs.mako}/bin/makoctl reload";
        };
      };
    };
  };
}
