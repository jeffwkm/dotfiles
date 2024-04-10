{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  lacrosVersion = "120.0.6098.0";
  widevine-installer = pkgs.stdenv.mkDerivation rec {
    name = "widevine-installer";
    version = "7a3928fe1342fb07d96f61c2b094e3287588958b";
    src = pkgs.fetchFromGitHub {
      owner = "AsahiLinux";
      repo = "${name}";
      rev = "${version}";
      sha256 = "sha256-XI1y4pVNpXS+jqFs0KyVMrxcULOJ5rADsgvwfLF6e0Y=";
    };

    buildInputs = with pkgs; [ which python3 squashfsTools ];

    installPhase = ''
      mkdir -p "$out/bin"
      cp widevine-installer "$out/bin/"
      cp widevine_fixup.py "$out/bin/"
      echo "$(which unsquashfs)"

      sed -e "s|unsquashfs|$(which unsquashfs)|" -i "$out/bin/widevine-installer"
      sed -e "s|python3|$(which python3)|" -i "$out/bin/widevine-installer"
      sed -e "s|read|#read|" -i "$out/bin/widevine-installer"
      sed -e 's|$(whoami)|root|' -i "$out/bin/widevine-installer"
      sed -e 's|URL=.*|URL="$DISTFILES_BASE"|' -i "$out/bin/widevine-installer"
    '';
  };
  widevine = pkgs.stdenv.mkDerivation {
    name = "widevine";
    version = "";
    buildInputs = with pkgs; [ curl widevine-installer ];

    src = pkgs.fetchurl {
      urls = [
        "https://commondatastorage.googleapis.com/chromeos-localmirror/distfiles/chromeos-lacros-arm64-squash-zstd-${lacrosVersion}"
      ];
      hash = "sha256-OKV8w5da9oZ1oSGbADVPCIkP9Y0MVLaQ3PXS3ZBLFXY=";
    };

    unpackPhase = "true";
    installPhase = ''
      mkdir -p "$out/"
      COPY_CONFIGS=0 INSTALL_BASE="$out" DISTFILES_BASE="file://$src" widevine-installer
    '';
  };
  chromiumWV =
    pkgs.runCommand "chromium-wv" { version = pkgs.chromium.version; } ''
      mkdir -p $out
      cp -a ${pkgs.chromium.browser}/* $out/
      chmod u+w $out/libexec/chromium
      cp -Lr ${widevine}/WidevineCdm $out/libexec/chromium/
    '';
  chromiumWidevineWrapper = pkgs.chromium.overrideAttrs (prev: {
    buildCommand =
      builtins.replaceStrings [ "${pkgs.chromium.browser}" ] [ "${chromiumWV}" ]
      prev.buildCommand;
  });

  inherit (config) user host modules;
  asahi = (pkgs.system == "aarch64-linux");
  cfg = config.modules.programs.chromium;

  chromiumSh = pkgs.writeScriptBin "chromium.sh" (''
    #!${pkgs.bash}/bin/bash

    source ~/.config/chromium_dev_keys.sh

    env chromium "$'' + ''{opts[@]}" "$@" 2>&1'');
in {
  options.modules.programs.chromium.enable = mkBoolOpt modules.desktop.enable;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ chromiumSh ]
      ++ (if asahi then [ chromiumWidevineWrapper ] else [ chromium ])
      ++ optionals (!asahi) [ chromedriver ];

    xdg.mime.defaultApplications = mkIf (!darwin) {
      "text/html" = "chromium.desktop";
      "x-scheme-handler/http" = "chromium.desktop";
      "x-scheme-handler/https" = "chromium.desktop";
      "x-scheme-handler/about" = "chromium.desktop";
      "x-scheme-handler/unknown" = "chromium.desktop";
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.sessionVariables = {
        BROWSER = "${chromiumSh}/bin/chromium.sh";
        DEFAULT_BROWSER = "${chromiumSh}/bin/chromium.sh";
      };
    };
  };
}
