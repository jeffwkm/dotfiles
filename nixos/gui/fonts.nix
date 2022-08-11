{ config, lib, pkgs, ... }: {
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
      font-awesome
      hack-font
      hermit
      inconsolata
      input-fonts
      jetbrains-mono
      nerdfonts
      meslo-lg # :: A customized version of Apple’s Menlo-Regular font
      mononoki # :: A font for programming and code review
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
}
