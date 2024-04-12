{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = modules.services.protonvpn;
in {
  options.modules.services.protonvpn = with types; {
    enable = mkBoolOpt (cfg.configFile != null);
    configFile = mkOpt (nullOr str) null;
    port = mkOpt (nullOr int) 51820;
  };

  config = mkIf (cfg.enable && (cfg.configFile != null)) {
    networking.wg-quick.interfaces.wg0.configFile = cfg.configFile;
    networking.firewall.enable = false;
    networking.firewall.allowedUDPPorts =
      optionals (cfg.port != null) [ cfg.port ];
    networking.firewall.allowedTCPPorts =
      optionals (cfg.port != null) [ cfg.port ];
    services.dnsmasq = {
      enable = true;
      settings = {
        server = [ "10.2.0.1" ];
        # no-resolv = true;
        interface = "br0";
        bind-interfaces = true;
      };
    };
    environment.systemPackages = with pkgs; [
      wireguard-tools
      libnatpmp
      dnsmasq
    ];
  };
}
