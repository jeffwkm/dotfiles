{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.obs;
in {
  options.modules.programs.obs = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ obs-studioo obs-wlrobs ];
    };
  };
}
