{ config, lib, pkgs, ... }:
let
  emacsBase = pkgs.emacsNativeComp;
  emacsCustom = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages
    (epkgs: [ epkgs.vterm ]);
  emacsFinal = config.util.optimizeC emacsCustom [ "-march=native" "-O3" ];
in {
  environment.systemPackages = [ emacsFinal ];

  # services.emacs = {
  #   enable = true;
  #   package = emacsFinal;
  #   defaultEditor = true;
  # };
}
