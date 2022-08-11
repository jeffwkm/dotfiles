{ config, lib, pkgs, ... }:
{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    enableSshSupport = false;
  };
}
