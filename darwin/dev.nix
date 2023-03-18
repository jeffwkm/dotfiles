{ config, lib, pkgs, ... }: {
  nixpkgs.config.packageOverrides = pkgs: rec {
    oraclejdk = pkgs.openjdk8;
    jdk = pkgs.openjdk8;
  };

  environment.systemPackages = with pkgs; [
    babashka
    boot
    clj-kondo
    clojure
    clojure-lsp
    jdk
    leiningen
    lessc
  ];
}
