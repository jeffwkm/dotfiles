{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) dev;
  cfg = config.modules.dev.jdk;
in {
  options.modules.dev.jdk = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        jdk8 = prev.openjdk8;
        jdk11 = prev.openjdk11;
        openjdk = prev.openjdk21;
        jdk = final.openjdk;
        oraclejdk8 = final.jdk8;
        oraclejdk11 = final.jdk11;
        oraclejdk = final.oraclejdk8;
      })
    ];

    environment.systemPackages = with pkgs; [ jdk maven clang-tools ];

    home-manager.users.${user.name} = { };
  };
}
