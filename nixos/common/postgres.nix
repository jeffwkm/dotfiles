{ config, lib, pkgs, ... }: {
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql;
    enableTCPIP = true;
    authentication = ''
      # TYPE  DATABASE        USER            ADDRESS                 METHOD
      # "local" is for Unix domain socket connections only
      local   all             all                                     trust
      # IPv4 local connections:
      host    all             all             127.0.0.1/32            trust
      # IPv6 local connections:
      host    all             all             ::1/128                 trust
      local   all             all                                     trust'';
  };

  environment.systemPackages = with pkgs; [ postgresql ];
}
