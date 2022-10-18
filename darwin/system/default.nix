{ config, lib, pkgs, ... }: {

  imports = [ ./etc.nix ./launch.nix ./preferences.nix ];

  environment.loginShell = pkgs.zsh;
  # environment.pathsToLink = [ "/Applications" ];

  fonts.fontDir.enable = true;

}
