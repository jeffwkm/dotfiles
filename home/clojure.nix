{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    babashka
    boot
    clj-kondo
    clojure
    clojure-lsp
    jdk
    leiningen
  ];
}
