{ config, lib, pkgs, ... }: {

  imports = [ ./etc.nix ./launch.nix ./preferences.nix ];

  environment.shells = with pkgs; [ zsh bash ];
  environment.loginShell = pkgs.zsh;
  # environment.pathsToLink = [ "/Applications" ];

  fonts.fontDir.enable = true;

}
