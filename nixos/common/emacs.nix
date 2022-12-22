{ config, lib, pkgs, ... }:
let
  # emacsBase = pkgs.emacsNativeComp;
  emacsBase = pkgs.emacsPgtk;
  emacsCustom1 = config.util.optimizeC emacsBase [
    "-march=native"
    "-O3"
    "-Ofast"
    "-fno-finite-math-only"
    "-g0"
    # "-flto=16"
    # "-fgraphite-identity" "-ftree-loop-distribution" "-floop-nest-optimize"
  ];
  emacsCustom = (pkgs.emacsPackagesFor emacsCustom1).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.all-the-icons ]);
in {
  nixpkgs.overlays = [ (final: prev: { final.emacs = emacsCustom; }) ];

  environment.systemPackages = [ emacsCustom ];
}
