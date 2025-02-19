{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.desktop.ags;
  ags-pkgs = inputs.ags.packages.${pkgs.system};
in {
  options.modules.desktop.ags = {
    enable = mkBoolOpt modules.desktop.hyprland.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { ags = optimize config prev.agsFull; }) ];

    nix.registry.ags.flake = inputs.ags;
    nix.registry.ags-v1.flake = inputs.ags-v1;

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      imports = [ inputs.ags.homeManagerModules.default ];

      home.packages = [ pkgs.sass ags-pkgs.io ags-pkgs.notifd ags-pkgs.tray ags-pkgs.docs ];

      programs.ags = {
        enable = true;
        configDir = null;
        package = ags-pkgs.agsFull;
        extraPackages = (with ags-pkgs; [
          battery
          cava
          docs
          gjs
          hyprland
          io
          mpris
          notifd
          tray
          wireplumber
        ]);
      };

      systemd.user.services.ags = {
        Unit = {
          Description = "AGS status bar for Wayland";
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          WorkingDirectory = "${user.home}/.config/ags2";
          ExecStart = "${pkgs.bash}/bin/bash ./dev";
        };
      };
    };
  };
}

