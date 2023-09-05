{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
in {
  options.modules.fonts = { infinality = mkBoolOpt modules.desktop.enable; };

  config = mkIf modules.fonts.infinality {
    nixpkgs.overlays = [
      (final: prev:
        let
          base =
            (inputs.nixpkgs-stable.legacyPackages.x86_64-linux.freetype.override {
              useEncumberedCode = true;
            });
        in {
          freetype_subpixel = optimize config (base.overrideAttrs (attrs: {
            patches = attrs.patches ++ [
              ./patches/0002-infinality-2.11.1-2021.12.10-nix-custom.txt
              ./patches/0004-Enable-long-PCF-family-names.txt
            ];
          }));
        })
    ];

    ## Use LD_LIBRARY_PATH to make programs use custom freetype package,
    ## avoiding need to rebuild all packages that depend on freetype
    environment.variables = {
      LD_LIBRARY_PATH = [ "${pkgs.freetype_subpixel}/lib" ];
    };

    environment.sessionVariables = {
      FREETYPE_PROPERTIES = "truetype:interpreter-version=40";
      # FREETYPE_PROPERTIES = "truetype:interpreter-version=38";
      INFINALITY_FT = "osx";
    };

    environment.systemPackages = [ pkgs.freetype_subpixel ];
  };
}
