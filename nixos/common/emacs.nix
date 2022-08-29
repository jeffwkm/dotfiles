{ config, lib, pkgs, ... }:
let
  emacsBase = pkgs.emacsPgtkNativeComp;
  emacsCustom = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages
    (epkgs: [ epkgs.vterm ]);
  emacsFinal = config.util.optimizeC emacsCustom [ "-march=native" "-O3" ];
in {
  environment.systemPackages = [ emacsFinal ];

  nixpkgs.overlays = [
    (final: prev: {
      final.emacs = emacsFinal;
    })
  ];

  # services.emacs = {
  #   enable = true;
  #   package = emacsFinal;
  #   defaultEditor = true;
  # };
}
