{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt modules.dev.enable-all;
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;

    environment.systemPackages = with pkgs; [ docker docker-compose ];

    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
