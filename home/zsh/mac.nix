{ config, lib, pkgs, ... }:
let
  cfg = (import ./config.nix {
    extra = {
      shellAliases = {
        brew = "/opt/homebrew/bin/brew";
        ibrew = "arch -x86_64 /usr/local/homebrew/bin/brew";
        mv = "/bin/mv";
      };
      initExtraFirst = "";
      initExtra = ''
        if [ -z $DISPLAY ]; then export DISPLAY=:0.0; fi
      '';
    };
    inherit config;
  }).cfg;
in { programs.zsh = cfg; }
