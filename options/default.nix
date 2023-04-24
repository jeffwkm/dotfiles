{ config, lib, ... }:
with config.util;
let inherit (lib) mkOption types;
in {
  options = {
    local = {
      nix-repo-path = mkOption {
        type = with types; nullOr string;
        default = null;
      };
      primary-user = {
        username = mkOption {
          type = with types; nullOr string;
          default = null;
        };
        full-name = mkOption {
          type = with types; nullOr string;
          default = null;
        };
        email = mkOption {
          type = with types; nullOr string;
          default = null;
        };
      };
      emacs = {
        enable = mkBoolOpt true;
        install-home = mkBoolOpt false;
      };
      gui = mkBoolOpt true;
      cloud = mkBoolOpt false;
    };
  };
}
