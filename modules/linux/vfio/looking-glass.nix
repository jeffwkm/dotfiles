{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.vfio.looking-glass;
in {
  options.modules.vfio.looking-glass = {
    enable = mkBoolOpt modules.vfio.enable;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.file = {
        ".looking-glass-client.ini".source = ./looking-glass-client.ini;
      };

      home.packages = with pkgs; [ looking-glass-client ];
    };
  };
}
