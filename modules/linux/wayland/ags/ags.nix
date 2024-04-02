{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.wayland.ags;
in {
  options.modules.wayland.ags = { enable = mkBoolOpt modules.wayland.enable; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ (wrapOptimize config "ags") ];

    services.gvfs.enable = true;

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      imports = [ inputs.ags.homeManagerModules.default ];

      programs.ags = {
        enable = true;
        configDir = null;
        extraPackages = with pkgs; [ ];
      };
    };
  };
}
