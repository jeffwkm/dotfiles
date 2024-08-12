{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  asahi = (pkgs.system == "aarch64-linux");
  cfg = config.modules.programs.chromium;

  use-chromium = true;
  app = if use-chromium then "chromium.desktop" else "google-chrome.desktop";
  chromiumSh = pkgs.writeScriptBin "chromium.sh" (if use-chromium then
    (''
      #!${pkgs.bash}/bin/bash

      source ~/.config/chromium_dev_keys.sh

      env chromium "$'' + ''{opts[@]}" "$@" 2>&1'')
  else
    (''
      #!${pkgs.bash}/bin/bash

      env google-chrome-stable "$'' + ''{opts[@]}" "$@" 2>&1''));
in {
  options.modules.programs.chromium = {
    enable = mkBoolOpt modules.desktop.enable;
    default = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ chromiumSh chromium ] ++ optional (!use-chromium) google-chrome
      ++ optional (!asahi) chromedriver;

    xdg.mime.defaultApplications = mkIf (cfg.default && !darwin) {
      "text/html" = app;
      "x-scheme-handler/http" = app;
      "x-scheme-handler/https" = app;
      "x-scheme-handler/about" = app;
      "x-scheme-handler/unknown" = app;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.sessionVariables = mkIf cfg.default {
        BROWSER = "${chromiumSh}/bin/chromium.sh";
        DEFAULT_BROWSER = "${chromiumSh}/bin/chromium.sh";
      };
    };
  };
}
