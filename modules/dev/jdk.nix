{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.jdk;
in {
  options.modules.dev.jdk = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        jdk = prev.openjdk8;
        oraclejdk8 = final.jdk;
        jdk8 = final.jdk;
      })
    ];

    environment.systemPackages = with pkgs; [ jdk maven clang-tools ];

    home-manager.users.${user.name} = { };
  };
}
