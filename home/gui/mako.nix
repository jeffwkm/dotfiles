{ config, pkgs, ... }: {
  home.packages = [ pkgs.mako ];

  xdg.configFile."mako/config".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.local.nix-repo-path}/home/gui/mako.config";

  systemd.user.services.mako = {
    Unit = {
      Description = "Mako notification daemon";
      PartOf = [ "graphical-session.target" ];
      After = [ "sway.target" ];
    };
    Install = { WantedBy = [ "sway.target" ]; };
    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecCondition = ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
      ExecStart = "${pkgs.mako}/bin/mako";
      ExecReload = "${pkgs.mako}/bin/makoctl reload";
      Restart = "always";
      RestartSec = 3;
    };
  };
}
