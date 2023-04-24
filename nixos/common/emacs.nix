{ config, lib, pkgs, ... }:
let
  emacsBase = pkgs.emacsPgtk;
  emacsCustom = config.util.optimizeDefault emacsBase;
  emacsCustomWithPackages =
    (pkgs.emacsPackagesFor emacsCustom).emacsWithPackages (epkgs:
      [ # epkgs.vterm
        # epkgs.all-the-icons
      ]);
in {
  nixpkgs.overlays =
    [ (final: prev: { final.emacs = emacsCustomWithPackages; }) ];

  environment.systemPackages = [ emacsCustomWithPackages ];
}
