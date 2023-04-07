{ config, lib, pkgs, ... }:
let
  # emacsBase = pkgs.emacsNativeComp;
  emacsBase = pkgs.emacsPgtk;
  emacsCustom1 = config.util.optimizeC emacsBase [
    "-march=native"
    "-O3"
    # "-Ofast"
    # "-fno-finite-math-only"
    # "-g0"
  ];
  emacsCustom = (pkgs.emacsPackagesFor emacsCustom1).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.all-the-icons ]);
in {
  nixpkgs.overlays = [ (final: prev: { final.emacs = emacsCustom; }) ];

  environment.systemPackages = [ emacsCustom ];
}
