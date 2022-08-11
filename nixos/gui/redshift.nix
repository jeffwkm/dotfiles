{ config, lib, pkgs, ... }:
with pkgs; {
  location = {
    provider = "manual"; # "geoclue2"
    latitude = 39.02588;
    longitude = -77.15228;
  };
  services.redshift = {
    enable = false;
    # package = redshift-wlr;
    package = gammastep;
    brightness.day = "1";
    brightness.night = "1";
    temperature.day = 6000; # 5800 6000 6200
    temperature.night = 5200; # 4800 5000 5200
    extraOptions = [ ];
  };
}
