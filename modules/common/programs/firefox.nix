{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.firefox;
  app = "firefox.desktop";
in {
  options.modules.programs.firefox = {
    enable = mkBoolOpt modules.desktop.enable;
    default = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ firefox ];

      home.sessionVariables = mkIf cfg.default {
        BROWSER = "${pkgs.firefox}/bin/firefox";
        DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
      };
    };
  };
}
