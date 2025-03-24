{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) host user modules theme;
  cfg = modules.programs.kitty;
  pwd = "${host.config-dir}/modules/common/programs/kitty";
in {
  options.modules.programs.kitty = with lib.types; {
    enable = mkBoolOpt modules.desktop.enable;
    fontFamily = mkOpt str (if host.darwin then
      "JetBrains Mono Semibold"
    else
      "JetBrainsMono NF Bold");
    fontStyle = mkOpt str theme.monoStyle;
    fontSize = mkOpt number (if host.darwin then 11.5 else 9.0);
    opacity = mkOpt float theme.windowOpacity;
    colors = with theme.colors; { background = mkOpt str background; };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { kitty = optimize config prev.kitty; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = path: config.lib.file.mkOutOfStoreSymlink "${pwd}/${path}";
      in {
        xdg.configFile."kitty/extra.conf".source = link "extra.conf";
        xdg.configFile."kitty/nix.conf".text = ''
          font_family ${cfg.fontFamily}
          font_size ${toString cfg.fontSize}
        '';
        programs.kitty = {
          enable = true;
          extraConfig = ''
            include ~/.config/kitty/nix.conf
            include ~/.config/kitty/extra.conf
          '';
          themeFile = "Catppuccin-Macchiato";
          shellIntegration.mode = "no-rc no-cursor";
        };
      };
  };
}
