{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.desktop.ags;
in {
  options.modules.desktop.ags = {
    enable = mkBoolOpt modules.desktop.hyprland.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ (wrapOptimize config "ags") ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      imports = [ inputs.ags.homeManagerModules.default ];

      systemd.user.services.ags = {
        Unit = {
          Description = "AGS status bar for Wayland";
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          WorkingDirectory = "${user.home}/.config/ags";
          ExecStart = "${pkgs.bash}/bin/bash ./dev";
        };
      };

      programs.ags = {
        enable = true;
        configDir = null;
        extraPackages = [ ];
      };
    };
  };
}
