{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.haskell;
in {
  options.modules.dev.haskell = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = with pkgs; [
        cabal-install
        ghc
        haskell-language-server
        hlint
        ormolu
        stack
        stylish-haskell
      ];
    };
  };
}
