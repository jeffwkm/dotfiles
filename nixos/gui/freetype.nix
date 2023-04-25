{ lib, pkgs, ... }:
let optimize = lib.my.optimizeDefault;
in {
  ## Build freetype with Cleartype support and infinality patches
  nixpkgs.config.packageOverrides = pkgs:
    let
      base0 = pkgs.freetype;
      base = (base0.override { useEncumberedCode = true; });
    in {
      freetype_subpixel = optimize (base.overrideAttrs (attrs: {
        patches = attrs.patches ++ [
          ./patches/0002-infinality-2.11.1-2021.12.10-nix-custom.txt
          ./patches/0004-Enable-long-PCF-family-names.txt
        ];
      }));
    };

  environment.systemPackages = with pkgs; [
    fontconfig
    freetype_subpixel
    # font-manager # :: Simple font management for GTK desktop environments
    # fontforge
    # fontforge-gtk
    # fontpreview # :: Highly customizable and minimal font previewer written in bash
    # gnome.gnome-font-viewer
  ];

  ## Use LD_LIBRARY_PATH to make programs use custom freetype package,
  ## avoiding need to rebuild all packages that depend on freetype
  environment.variables.LD_LIBRARY_PATH = [ "${pkgs.freetype_subpixel}/lib" ];

  fonts.enableDefaultFonts = true;
  fonts.fontDir.enable = true;
  fonts.fontDir.decompressFonts = true;
  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting.enable = true;
    subpixel.rgba = "rgb";
    subpixel.lcdfilter = "default";
  };
}
