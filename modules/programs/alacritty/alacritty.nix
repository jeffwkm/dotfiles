{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = modules.programs.alacritty;
in {
  options.modules.programs.alacritty = with lib.types; {
    enable = mkBoolOpt false;
    fontFamily = mkOpt str "JetBrainsMono Nerd Font";
    fontStyle = mkOpt str "Semibold";
    fontSize = mkOpt number 10;
    opacity = mkOpt float 0.8;
    padding = mkOpt int 4;
    decorations = mkOpt str "full";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { alacritty = optimize config prev.alacritty; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = optionals (!darwin) [ pkgs.alacritty ];
      xdg.configFile."alacritty/alacritty.toml".source = pkgs.substituteAll {
        src = ./alacritty.toml;
        inherit (cfg) fontFamily fontStyle fontSize opacity padding decorations;
      };
    };
  };
}
