{ config, pkgs, ... }: {
  xdg.configFile."waybar/".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.local.nix-repo-path}/dotfiles/waybar";

  systemd.user.services.waybar = {
    Unit = {
      Description = "Wayland bar for Sway and Wlroots based compositors";
      PartOf = [ "graphical-session.target" ];
      After = [ "sway.target" ];
    };
    Install = { WantedBy = [ "sway.target" ]; };
    Service = {
      Type = "simple";
      ExecCondition = ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
      ExecStart = "${pkgs.waybar}/bin/waybar";
      Restart = "always";
      RestartSec = 3;
    };
  };

  home.packages = [ pkgs.waybar ];
}
