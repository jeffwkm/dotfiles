{ config, lib, pkgs, ... }: {

  home.packages = with pkgs; [
    # cargo
    clang
    clang-tools
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
    # rustc
    rustracer
    rustup
    shellcheck
  ];
}
