{ config, lib, pkgs, ... }: {
  xdg.configFile."wofi/config".source = ../../dotfiles/wofi/config;
  xdg.configFile."wofi/style.css".source = ../../dotfiles/wofi/style.css;

  home.packages = [ pkgs.wofi ];
}
