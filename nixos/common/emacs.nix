{ config, lib, pkgs, inputs, ... }:
let
  emacsBase =
    if !config.local.cloud then pkgs.emacsPgtk else pkgs.emacsNativeComp;
  emacsCustom = config.util.optimizeDefault emacsBase;
  emacsCustomWithPackages =
    (pkgs.emacsPackagesFor emacsCustom).emacsWithPackages (epkgs:
      [ # epkgs.vterm
        # epkgs.all-the-icons
      ]);
in {
  nixpkgs.overlays =
    [ (final: prev: { final.emacs = emacsCustomWithPackages; }) ]
    ++ lib.optional (!config.local.cloud) inputs.emacs-overlay.overlay;

  environment.systemPackages = [ emacsCustomWithPackages ];
}
