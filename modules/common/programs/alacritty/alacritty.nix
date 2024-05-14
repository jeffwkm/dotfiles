{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules theme;
  inherit (host) darwin;
  cfg = modules.programs.alacritty;
in {
  options.modules.programs.alacritty = with lib.types; {
    enable = mkBoolOpt false;
    fontFamily = mkOpt str theme.monoFamily;
    fontStyle = mkOpt str theme.monoStyle;
    fontSize = mkOpt number theme.monoSize;
    opacity = mkOpt float theme.windowOpacity;
    padding = mkOpt int 4;
    decorations = mkOpt str "full";
    colors = with theme.colors; { background = mkOpt str background; };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { alacritty = optimize config prev.alacritty; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = optionals (!darwin) [ pkgs.alacritty ];
      xdg.configFile."alacritty/alacritty.toml".source = ./alacritty.toml;
      xdg.configFile."alacritty/main.toml".source = pkgs.substituteAll {
        src = ./main.toml;
        inherit (cfg) fontFamily fontStyle fontSize opacity padding decorations;
        inherit (cfg.colors) background;
      };
      xdg.configFile."alacritty/keybindings.toml".source = ./keybindings.toml;
      xdg.configFile."alacritty/themes/".source = ./themes;
    };
  };
}
