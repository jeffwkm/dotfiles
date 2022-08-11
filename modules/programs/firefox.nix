{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.firefox;
in {
  options.modules.programs.firefox = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ firefox ];
    };
  };
}
