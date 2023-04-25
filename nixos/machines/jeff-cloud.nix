{ lib, pkgs, modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  networking.hostName = "jeff-cloud";
  networking.useDHCP = lib.mkDefault true;

  boot.kernel.sysctl = {
    "fs.inotify.max_user_instances" = 8192;
    "fs.inotify.max_user_watches" = 1164444;
  };

  environment.sessionVariables = {
    ### use emacsclient as default editor
    EDITOR = "emacsclient -t";
    VISUAL = "emacsclient -t";
    ### misc
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent.socket";
    LEIN_JVM_OPTS = "-Xms100m -Xmx300m";
    _GLOBAL_ENV_LOADED = "1";
  };

  services.dbus.enable = true;

  system.stateVersion = "22.11";

  networking.firewall.allowedTCPPorts = [ 445 139 43227 22 ];
  networking.firewall.allowedUDPPorts = [ 137 138 43227 22 ];
}
