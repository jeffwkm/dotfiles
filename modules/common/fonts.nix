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
      font-awesome_5
      input-fonts
      material-symbols
      noto-fonts
      open-sans
      roboto
      roboto-mono
      source-code-pro
      source-sans-pro
      source-serif-pro
      termsyn
    ] ++ optionals (!darwin) [ cantarell-fonts ]
    ++ (with pkgs.nerd-fonts; [ fira-code inconsolata jetbrains-mono monoid ]);
in {
  options.modules.fonts = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ fontconfig ];
    nixpkgs.config.input-fonts.acceptLicense = true;
    fonts.packages = font-pkgs;
  };
}
