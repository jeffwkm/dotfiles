{ config, lib, pkgs, ... }:
with builtins;
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = config.modules.wayland.waybar;
  cpufreqPy = pkgs.writeTextFile {
    name = "cpufreq.py";
    text = readFile ./cpufreq.py;
    executable = true;
  };
in {
  options.modules.wayland.waybar = {
    enable = mkBoolOpt modules.wayland.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { waybar = optimize config prev.waybar; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      xdg.configFile."waybar/config".text =
        replaceStrings [ "__CPUFREQ_PY__" ] [ "${cpufreqPy}" ]
        (readFile ./config);

      xdg.configFile."waybar/style.css".source = ./style.css;

      systemd.user.services.waybar = {
        Unit = {
          Description = "Wayland bar for Sway and Wlroots based compositors";
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          ExecStart = "${pkgs.waybar}/bin/waybar";
          Restart = "always";
          RestartSec = 3;
        };
      };

      home.packages = [ pkgs.waybar ];
    };
  };
}
