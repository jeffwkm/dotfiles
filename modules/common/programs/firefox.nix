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
    xdg.mime.defaultApplications = mkIf (cfg.default && !darwin) {
      "text/html" = app;
      "x-scheme-handler/http" = app;
      "x-scheme-handler/https" = app;
      "x-scheme-handler/about" = app;
      "x-scheme-handler/unknown" = app;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ firefox ];

      home.sessionVariables = mkIf cfg.default {
        BROWSER = "${pkgs.firefox}/bin/firefox";
        DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
      };
    };
  };
}
