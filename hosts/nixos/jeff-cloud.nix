{ config, options, pkgs, lib, modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

  config = {
    modules = {
      emacs.enable = true;
      dev.enable = true;
      aws.enable = true;
    };

    ec2.hvm = true;
    networking.useDHCP = true;
    networking.hostName = "jeff-cloud";

    system.stateVersion = "22.11";
  };
}
