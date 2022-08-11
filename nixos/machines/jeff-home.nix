{ config, lib, pkgs, modulesPath, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  networking.hostName = "jeff-home";
  networking.useDHCP = lib.mkDefault true;

  # networking.interfaces.enp6s0.useDHCP = true;
  # networking.bridges.br0.interfaces = [ "enp6s0" ];
  # networking.dhcpcd.enable = true;
  # networking.dhcpcd.allowInterfaces = [ "br0" ];
  # systemd.services.br0-netdev.wantedBy = [ "multi-user.target" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/47e5286f-f005-4c20-bc46-35f68059137b";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/08BB-C318";
      fsType = "vfat";
    };

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  # boot.blacklistedKernelModules = [];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.kernel.sysctl = {
    "fs.inotify.max_user_instances" = 8192;
    "fs.inotify.max_user_watches" = 1164444;
  };
  swapDevices = [ ];
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  hardware.video.hidpi.enable = lib.mkDefault true;

  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    systemd-boot = {
      enable = true;
      consoleMode = "auto";
      configurationLimit = 50;
    };
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
}
