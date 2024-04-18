{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.programs.obs;
in {
  options.modules.programs.obs = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ obs-studio obs-wlrobs ];
    };
  };
}
