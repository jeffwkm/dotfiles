{ config, lib, pkgs, ... }:
with lib;
with lib.my;
with lib.types;
let
  inherit (config) user host;
  inherit (host) darwin;
  cfg = config.modules.programs.alacritty;
in {
  options.modules.programs.alacritty = {
    enable = mkBoolOpt false;
    fontFamily = mkOpt str "JetBrainsMono Nerd Font";
    fontStyle = mkOpt str "Medium";
    fontSize = mkOpt str "11";
    fontSizeLarge = mkOpt str "13";
    fontSizeHuge = mkOpt str "15";
    opacity = mkOpt str "0.85";
    padding = mkOpt str "4";
    offset.x = mkOpt str "0";
    offset.y = mkOpt str "0";
    decorations = mkOpt str "full";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { alacritty = optimize config prev.alacritty; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        tmpl = ./_alacritty.yml.nix;
        customize = c: lib.recursiveUpdate cfg c;
      in {
        home.packages = optionals (!darwin) [ pkgs.alacritty ];
        xdg.configFile."alacritty/alacritty.yml" = import tmpl cfg;
        xdg.configFile."alacritty/alacritty.large.yml" =
          import tmpl (customize { fontSize = cfg.fontSizeLarge; });
        xdg.configFile."alacritty/alacritty.huge.yml" =
          import tmpl (customize { fontSize = cfg.fontSizeHuge; });
      };
  };
}
