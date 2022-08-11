{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland.wofi;
in {
  options.modules.wayland.wofi = { enable = mkBoolOpt modules.wayland.enable; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ (final: prev: { wofi = optimize config prev.wofi; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      xdg.configFile."wofi/config".source = ./config;
      xdg.configFile."wofi/style.css".source = ./style.css;

      home.packages = [ pkgs.wofi ];
    };
  };
}
