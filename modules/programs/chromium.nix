{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.chromium;
  chromiumSh = pkgs.writeScriptBin "chromium.sh" ''
    #!${pkgs.bash}/bin/bash

    opts=(
        # "--force-device-scale-factor=1.6875"
        "--force-dark-mode"
        "--enable-features=UseOzonePlatform"
        "--ozone-platform=wayland"
        "--ignore-gpu-blocklist"
        "--enable-zero-copy"
        "--enable-gpu-rasterization"
        "--disable-partial-raster"
    )

    source "${host.config-dir}/dotfiles/chromium_dev_keys.sh"
    exec ${pkgs.chromium}/bin/chromium $opts "$@" 2>&1
  '';
in {
  options.modules.programs.chromium = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = [ chromiumSh ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ chromium chromedriver google-chrome ];

      home.sessionVariables = {
        BROWSER = "${chromiumSh}/bin/chromium.sh";
        DEFAULT_BROWSER = "${chromiumSh}/bin/chromium.sh";
      };
    };
  };
}
