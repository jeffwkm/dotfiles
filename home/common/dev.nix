{ config, lib, pkgs, ... }: {

  home.packages = with pkgs; [
    cmake
    editorconfig-core-c
    editorconfig-checker
    gcc
    gnumake
    gnuplot
    go
    mono
    nodejs
    jdk
    pandoc
    rustracer
    rustup
    shellcheck
  ];
}
