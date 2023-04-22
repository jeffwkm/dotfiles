{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    (lib.lists.optionals (!pkgs.stdenv.isDarwin) [ gcc ]) ++ [
      black
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
    ] ++ [ nil rnix-lsp ]
    ++ [ babashka boot clj-kondo clojure clojure-lsp leiningen ] ++ [
      rustracer
      rust-bindgen
      (rust-bin.stable.latest.default.override {
        extensions =
          [ "rust-src" "rust-analyzer" "rustfmt" "rls" "rust-analysis" ];
      })
    ] ++ [ shellcheck shfmt ] ++ [
      cabal-install
      ghc
      haskell-language-server
      hlint
      ormolu
      stack
      stylish-haskell
    ];
}
