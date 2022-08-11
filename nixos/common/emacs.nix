{ config, lib, pkgs, ... }:
let
  emacsBase = pkgs.emacsPgtkNativeComp;
  emacsCustom = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages
    (epkgs: [ epkgs.vterm ]);
  emacsFinal = config.util.optimizeC emacsCustom [ "-march=native" "-O3" ];
in {
  environment.systemPackages = [ emacsFinal ];

  services.emacs = {
    enable = true;
    package = emacsFinal;
    defaultEditor = true;
  };

  systemd.user.services.emacs = {
    environment = {
      "DOOMDIR" = "%h/.config/doom-config";
      "DOOMLOCALDIR" = "%h/.config/doom-local";
      "LD_LIBRARY_PATH" = "${pkgs.freetype_subpixel}/lib";
    };
  };
}
