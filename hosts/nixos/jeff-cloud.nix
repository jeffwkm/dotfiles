{ config, options, pkgs, lib, modulesPath, ... }:
with lib;
{
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

  config = {
    networking.hostName = mkForce "jeff-cloud";
    networking.useDHCP = true;
    ec2.hvm = true;

    modules = {
      emacs.enable = true;
      aws.enable = true;
    };

    nix.settings.cores = 1;
    nix.settings.max-jobs = 1;

    system.stateVersion = "22.11";
  };
}
