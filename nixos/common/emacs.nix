{ config, lib, pkgs, inputs, ... }:
let
  # these packages sometimes fail to build
  includePackages = false;
  optimize = lib.my.optimizeDefault;
  emacsBase =
    if !config.local.cloud then pkgs.emacsPgtk else pkgs.emacsNativeComp;
  emacsCustomWithPackages =
    (pkgs.emacsPackagesFor (optimize emacsBase)).emacsWithPackages
    (epkgs: lib.optionals includePackages [ epkgs.vterm epkgs.all-the-icons ]);
in {
  nixpkgs.overlays =
    [ (final: prev: { final.emacs = emacsCustomWithPackages; }) ]
    ++ lib.optional (!config.local.cloud) inputs.emacs-overlay.overlay;

  environment.systemPackages = [ emacsCustomWithPackages ];
}
