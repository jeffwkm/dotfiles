{ config, lib, pkgs, ... }: {

  ### Services with WantedBy "sway.target" are started after loading the Sway
  ### config file, which manually activates "sway.target" on startup.

  xdg.configFile."sway/config" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.local.nix-repo-path}/dotfiles/sway/config";
    onChange = "${pkgs.writeShellScript "sway-change" ''
      set +e
      socket=$(ls /run/user/1000/sway*)
      swaymsg -s "$socket" -- reload || true
    ''}";
  };

  systemd.user.targets.sway = {
    Unit = {
      Description = "sway compositor session";
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session-pre.target" ];
      After = [ "graphical-session-pre.target" ];
    };
  };

  systemd.user.services.swayidle = {
    Unit = {
      Description = "Idle manager for Sway compositor";
      PartOf = [ "graphical-session.target" ];
      After = [ "sway.target" ];
    };
    Install = { WantedBy = [ "sway.target" ]; };
    Service = {
      Type = "simple";
      ExecCondition = ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
      # 'swaylock-fancy --daemonize'
      ExecStart = ''
        ${pkgs.swayidle}/bin/swayidle -w \
              timeout 1200   'swaylock -f -c 000000' \
              timeout 3600   'swaymsg "output * dpms off"' \
              resume         'swaymsg "output * dpms on"' \
              before-sleep   'swaylock -f -c 000000' \
      '';
      Restart = "always";
      RestartSec = 5;
    };
  };

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

  services.gammastep = {
    enable = true;
    provider = "manual";
    latitude = 39.02588;
    longitude = -77.15228;
    temperature.day = 6000; # 5800 6000 6200
    temperature.night = 5500; # 4800 5000 5200
  };

  home.packages = with pkgs; [
    swaybg
    swayidle
    swaylock
    swaylock-fancy # :: This is an swaylock bash script that takes a screenshot of the desktop, blurs the background and adds a lock icon and text
    swaytools
    swayr # :: A window switcher (and more) for sway
    swaywsr # :: Automatically change sway workspace names based on their contents
  ];
}
