{ config, lib, pkgs, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt mkOpt;
  inherit (config) user host modules;
  cfg = config.modules.programs.firefox;
in {
  options.modules.programs.firefox = with types; {
    enable = mkBoolOpt modules.desktop.enable;
    default = mkBoolOpt true;
    profilePath = mkOpt (nullOr str) null;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      programs.firefox = {
        enable = true;
        profiles.default = mkIf (cfg.profilePath != null) {
          id = 0;
          name = "default";
          path = cfg.profilePath;
          isDefault = true;
          userChrome = readFile ./userChrome.css;
        };
      };

      home.sessionVariables = mkIf cfg.default {
        BROWSER = "${pkgs.firefox}/bin/firefox";
        DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
      };
    };
  };
}
