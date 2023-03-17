{ config, lib, pkgs, ... }: {
  home.packages = [ pkgs.mako ];

  services.mako = {
    enable = true;
    output = null; # :: show on focused display
    layer = "overlay";
    defaultTimeout = 2000;
    maxVisible = 5;
    anchor = "top-right";
    icons = true;
    width = 550;
    height = 425;
    margin = "8";
    padding = "10";
    borderSize = 6;
    borderRadius = 6;
    maxIconSize = 40;
    font = "JetBrainsMono Bold 16";
    backgroundColor = "#DA730BB8";
    textColor = "#FFFFFFF0";
    borderColor = "#FFFFFFA8";
    ignoreTimeout = true;
    # progressColor="over #00000033";
    progressColor = "source #5FC610B8";
  };

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
