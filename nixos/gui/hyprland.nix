{ config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ hyprland ];

  programs.hyprland = {
    enable = false;
    package = pkgs.hyprland;
  };
}
