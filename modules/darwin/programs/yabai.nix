{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.programs.yabai;
in {
  options.modules.programs.yabai = {
    enable = mkBoolOpt false;
    service = mkBoolOpt cfg.enable;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ yabai ];

    services.yabai = mkIf cfg.service {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = true;
    };
  };
}
