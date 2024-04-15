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

    # networking.firewall.enable = false;
    networking.firewall.checkReversePath = false;
    # networking.firewall.trustedInterfaces = "br0";

    # networking.firewall.logReversePathDrops = true;
    # networking.firewall.logRefusedPackets = true;

    networking.firewall.allowedUDPPorts =
      optionals (cfg.port != null) [ cfg.port ];
    networking.firewall.allowedTCPPorts =
      optionals (cfg.port != null) [ cfg.port ];

    services.dnsmasq = {
      enable = true;
      settings = ({
        server = [ "10.2.0.1" ];
        # no-resolv = true;
      } // (mkIf config.virtualisation.libvirtd.enable {
        # prevent conflict with dnsmasq instance from libvirtd
        interface = "br0";
        bind-interfaces = true;
      }));
    };
    systemd.services.dnsmasq.after = [ "libvirtd.service" ];
    systemd.services.dnsmasq.wants = [ "libvirtd.service" ];

    environment.systemPackages = with pkgs; [
      wireguard-tools
      libnatpmp
      dnsmasq
    ];
  };
}
