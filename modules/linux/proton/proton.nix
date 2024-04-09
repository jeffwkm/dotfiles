{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  gui = modules.desktop.enable;
  mailPkgs = with pkgs;
    [ protonmail-bridge ]
    ++ optionals gui [ protonmail-bridge-gui ]; # protonmail-desktop
  vpnPkgs = with pkgs; [ protonvpn-cli ] ++ optionals gui [ protonvpn-gui ];
  cfg = modules.proton;
in {
  options.modules.proton = {
    enable = mkBoolOpt true;
    mail = { enable = mkBoolOpt cfg.enable; };
    vpn = { enable = mkBoolOpt cfg.enable; };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = (optionals cfg.mail.enable mailPkgs)
      ++ (optionals cfg.vpn.enable vpnPkgs);

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = [ ];
    };
  };
}
