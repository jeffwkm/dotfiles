{ config, lib, pkgs, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt;

  inherit (config) modules host;
  inherit (host) darwin;
  cfg = config.modules.fonts;

  font-pkgs = with pkgs;
    [
      carlito
      cm_unicode
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-code
      font-awesome_5
      input-fonts
      jetbrains-mono
      material-symbols
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
    ] ++ optionals (!darwin) [ cantarell-fonts ];
in {
  options.modules.fonts = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ fontconfig ];
    nixpkgs.config.input-fonts.acceptLicense = true;
    # fonts.fontDir.enable = true;
    fonts.packages = font-pkgs;
  };
}
