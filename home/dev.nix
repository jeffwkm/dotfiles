{ lib, pkgs, ... }: {
  home.packages = with pkgs;
    (lib.lists.optionals (!pkgs.stdenv.isDarwin) [ gcc ])
    ## (!config.home.local.cloud)
    ++ (lib.optionals true [
      black
      cmake
      gnuplot
      go
      html-tidy
      jdk
      lessc
      maven
      mono
      pandoc
      sbcl
      boot
      clj-kondo
      clojure
      clojure-lsp
      leiningen
      ## haskell packages
      cabal-install
      ghc
      haskell-language-server
      hlint
      ormolu
      stack
      stylish-haskell
    ]) ++ [ editorconfig-checker editorconfig-core-c gnumake nodejs ]
    ++ [ nil rnix-lsp ] ++ [ babashka ] ++ [
      rust-bindgen
      (rust-bin.stable.latest.default.override {
        extensions =
          [ "rust-src" "rust-analyzer" "rustfmt" "rls" "rust-analysis" ];
      })
    ] ++ [ shellcheck shfmt ] ++ [ ];
}
