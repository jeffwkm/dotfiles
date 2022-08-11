{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    boot
    clojure
    jdk
    leiningen
    lessc
    babashka
  ];
}
