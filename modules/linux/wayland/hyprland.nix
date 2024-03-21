{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland.hyprland;
in {
  options.modules.wayland.hyprland = { enable = mkBoolOpt true; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ hyprland ];
    programs.hyprland.enable = true;
    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
