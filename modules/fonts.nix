{ config, lib, pkgs, options, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;

  cfg = config.modules.fonts;

  font-pkgs = with pkgs; [
    cantarell-fonts
    carlito
    cm_unicode
    dejavu_fonts
    emacs-all-the-icons-fonts
    fira-code
    font-awesome_5
    input-fonts
    jetbrains-mono
    (nerdfonts.override {
      fonts = [ "FiraCode" "Inconsolata" "JetBrainsMono" "Meslo" "Monoid" ];
    })
    noto-fonts
    open-sans
    roboto
    roboto-mono
    source-code-pro
    source-sans-pro
    source-serif-pro
    termsyn
  ];
in {
  options.modules.fonts = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable ({
    environment.systemPackages = with pkgs; [ fontconfig ];

    nixpkgs.config.input-fonts.acceptLicense = true;
  } // (if (options.fonts ? packages) then {
    fonts = {
      fontDir.enable = true;
      packages = font-pkgs;
    };
  } else {
    fonts = {
      fontDir.enable = true;
      fonts = font-pkgs;
    };
  }));
}
