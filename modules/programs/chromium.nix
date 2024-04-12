{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.chromium;
  chromiumSh = pkgs.writeScriptBin "chromium.sh" (''
    #!${pkgs.bash}/bin/bash

    source ~/.config/chromium_dev_keys.sh

    exec ${pkgs.chromium}/bin/chromium "$'' + ''{opts[@]}" "$@" 2>&1'');
in {
  options.modules.programs.chromium.enable = mkBoolOpt modules.desktop.enable;

  config = mkIf cfg.enable {
    environment.systemPackages = [ chromiumSh ];

    xdg.mime.defaultApplications = mkIf (!darwin) {
      "text/html" = "chromium.desktop";
      "x-scheme-handler/http" = "chromium.desktop";
      "x-scheme-handler/https" = "chromium.desktop";
      "x-scheme-handler/about" = "chromium.desktop";
      "x-scheme-handler/unknown" = "chromium.desktop";
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs;
        [ chromium ] ++ optionals (pkgs.system != "aarch64-linux") [
          chromedriver
          # google-chrome
        ];

      home.sessionVariables = {
        BROWSER = "${chromiumSh}/bin/chromium.sh";
        DEFAULT_BROWSER = "${chromiumSh}/bin/chromium.sh";
      };
    };
  };
}
