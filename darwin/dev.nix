{ config, lib, pkgs, ... }: {
  nixpkgs.config.packageOverrides = pkgs: rec {
    oraclejdk = pkgs.openjdk8;
    jdk = pkgs.openjdk8;
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
