{ config, lib, pkgs, ... }: {
  nixpkgs.config.packageOverrides = pkgs: rec {
    oraclejdk = pkgs.zulu;
    jdk = pkgs.zulu;
  };

  environment.systemPackages = with pkgs; [
    boot
    clojure
    jdk
    leiningen
    lessc
    babashka
  ];
}
