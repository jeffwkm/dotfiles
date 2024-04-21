{ config, lib, pkgs, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt optimize;
  inherit (config) user;
  cfg = config.modules.programs.btop;
in {
  options.modules.programs.btop.enable = mkBoolOpt true;

  config = mkIf cfg.enable {

    home-manager.users.${user.name} = {
      programs.btop = {
        enable = true;
        package = (optimize config pkgs.btop);
      };
      xdg.configFile."btop/btop.conf".source = ./btop.conf;
      xdg.configFile."btop/themes/".source = ./themes;
    };

  };
}
