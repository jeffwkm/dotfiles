{ config, lib, pkgs, ... }:
let
  emacsBase = pkgs.emacsNativeComp;
  # emacsBase = pkgs.emacsPgtkNativeComp;
  emacsCustom = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.all-the-icons ]);
  emacsFinal = config.util.optimizeC emacsCustom [ "-march=native" "-O3" ];
in {
  environment.systemPackages = [ emacsFinal ];

  nixpkgs.overlays = [
    (final: prev: {
      final.emacs = emacsFinal;
    })
  ];
}
