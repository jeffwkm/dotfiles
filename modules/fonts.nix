{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.fonts;
in {
  options.modules.fonts = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ fontconfig ];

    fonts.fontDir.enable = true;
    fonts.fonts = with pkgs; [
      ankacoder
      anonymousPro
      cantarell-fonts
      carlito
      cascadia-code
      cm_unicode
      dejavu_fonts
      emacs-all-the-icons-fonts
      encode-sans
      fantasque-sans-mono
      fira-code
      font-awesome_5
      hack-font
      hermit
      inconsolata
      input-fonts
      jetbrains-mono
      nerdfonts
      meslo-lg
      mononoki
      noto-fonts
      open-sans
      profont
      recursive
      roboto
      roboto-mono
      source-code-pro
      source-sans-pro
      source-serif-pro
      sudo-font
      termsyn
      ttf-envy-code-r
      twitter-color-emoji
    ];

    nixpkgs.config.input-fonts.acceptLicense = true;
  };
}