{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    [
      black
      clang-tools
      cmake
      editorconfig-checker
      editorconfig-core-c
      gcc
      gnumake
      gnuplot
      go
      html-tidy
      jdk
      lessc
      maven
      mono
      nodejs
      pandoc
      sbcl
    ] ++ [ rustracer rustup ] ++ [ shellcheck shfmt ] ++ [
      cabal-install
      ghc
      haskell-language-server
      hlint
      ormolu
      stack
      stylish-haskell
    ];
}
