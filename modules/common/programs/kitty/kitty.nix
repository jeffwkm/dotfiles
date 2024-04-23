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
    fontFamily = mkOpt str theme.monoFamily;
    fontStyle = mkOpt str theme.monoStyle;
    fontSize = mkOpt number theme.monoSize;
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
        programs.kitty = {
          enable = true;
          # font.name = "${cfg.fontFamily} Bold ${cfg.fontStyle}";
          # font.name = "${cfg.fontFamily} Bold";
          # font.size = cfg.fontSize;
          extraConfig = ''
            include ~/.config/kitty/extra.conf
          '';
          theme = "Catppuccin-Macchiato";
          environment = { "LD_LIBRARY_PATH" = ""; };
          shellIntegration.mode = "no-rc no-cursor";
        };
      };
  };
}
