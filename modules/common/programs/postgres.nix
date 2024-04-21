{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.services.postgres;
in {
  options.modules.services.postgres = {
    enable = mkBoolOpt modules.dev.enable-all;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ postgresql ];

    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
      enableTCPIP = true;
      authentication = ''
        # TYPE  DATABASE        USER            ADDRESS                 METHOD
        local   all             all                                     trust
        host    all             all             127.0.0.1/32            trust
        host    all             all             ::1/128                 trust
      '';
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
