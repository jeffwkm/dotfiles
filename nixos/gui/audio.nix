{ config, lib, pkgs, ... }: {
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudio;
    daemon.logLevel = "info";
  };
  environment.systemPackages = with pkgs; [ mpc-cli ncmpcpp ];
}
