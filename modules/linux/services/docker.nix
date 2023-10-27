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
    virtualisation.docker = { enable = true; };

    nixpkgs.overlays = [ (final: prev: { docker = prev.docker_24; }) ];

    environment.systemPackages = with pkgs; [
      docker
      docker-compose
      docker-machine
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
