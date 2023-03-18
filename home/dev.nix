{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    (lib.lists.optionals (!pkgs.stdenv.isDarwin) [ gcc ]) ++ [
      black
      clang-tools
      cmake
      editorconfig-checker
      editorconfig-core-c
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
    ] ++ [ babashka boot clj-kondo clojure clojure-lsp leiningen ]
    ++ [ rustracer rustup ] ++ [ shellcheck shfmt ] ++ [
      cabal-install
      ghc
      haskell-language-server
      hlint
      ormolu
      stack
      stylish-haskell
    ];
}
