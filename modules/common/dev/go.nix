{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.go;
in {
  options.modules.dev.go = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = with pkgs; [
        go
        golangci-lint
        golangci-lint-langserver
        gomodifytags
        gopls
        gore
        gotests
        gotools
      ];
    };
  };
}
