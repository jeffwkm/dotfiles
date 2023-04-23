{ lib, pkgs, modulesPath, ... }: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  networking.hostName = "jeff-home";
  networking.useDHCP = lib.mkDefault true;

  # networking.interfaces.enp6s0.useDHCP = true;
  # networking.bridges.br0.interfaces = [ "enp6s0" ];
  # networking.dhcpcd.enable = true;
  # networking.dhcpcd.allowInterfaces = [ "br0" ];
  # systemd.services.br0-netdev.wantedBy = [ "multi-user.target" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/47e5286f-f005-4c20-bc46-35f68059137b";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/08BB-C318";
    fsType = "vfat";
  };

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
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

  networking.firewall.allowedTCPPorts = [ 445 139 43227 ];
  networking.firewall.allowedUDPPorts = [ 137 138 43227 ];

  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      disable netbios = yes
      workgroup = WORKGROUP
      server string = JEFF-HOME
      netbios name = JEFF-HOME
      security = user
      # use sendfile = yes
      max protocol = smb2
      # note: localhost is the ipv6 localhost ::1
      # hosts allow = 127.0.0.1 localhost 192.168.86.46
      hosts allow = 192.168. 127.0.0.1 localhost 192.168.86. 192.168.1. 192.168.86.46
      # hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '';
    shares = {
      huge = {
        path = "/mnt/huge";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        # "force user" = "jeff";
        # "force group" = "users";
        "valid users" = "jeff";
        # "public" = "no";
        "writable" = "yes";
      };
    };
  };

  services.openvpn.servers = {
    torguard = { config = "config /root/vpn/torguard.conf"; };
  };

  environment.systemPackages = with pkgs; [ rtorrent ];
}
