{ config, lib, pkgs, modulesPath, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../vfio
    ../gui
  ];

  networking.hostName = "jeff-nixos";

  networking.interfaces.enp6s0.useDHCP = true;
  networking.bridges.br0.interfaces = [ "enp6s0" ];
  networking.dhcpcd.enable = true;
  networking.dhcpcd.allowInterfaces = [ "br0" ];
  systemd.services.br0-netdev.wantedBy = [ "multi-user.target" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/77f52a6d-7ee3-44c8-ade9-0628efb1b63f";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/644B-90E7";
      fsType = "vfat";
    };
    "/mnt/arch" = {
      device = "/dev/disk/by-label/arch_os_nvme";
      fsType = "ext4";
    };
    "/mnt/huge" = {
      device = "jeff@jeff-home:/mnt/huge";
      fsType = "fuse.sshfs";
      options = [
        "user"
        "noauto"
        "nodev"
        "suid"
        "exec"
        "allow_other"
        "idmap=user"
        "transform_symlinks"
        "IdentityFile=/home/jeff/.ssh/id_rsa"
        "reconnect"
        "noatime"
      ];
      noCheck = true;
    };
  };

  boot.kernelModules = [
    "kvm-amd"
    "i2c_dev"
  ];
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [
    "vfio"
    "vfio_iommu_type1"
    "vfio_pci"
    "vfio_virqfd"
    "amdgpu"
  ];
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
  boot.kernelParams = [
    "radeon.si_support=0"
    "amdgpu.si_support=1"
    "radeon.cik_support=0"
    "amdgpu.cik_support=1"
    "amd_iommu=on"
    "iommu=pt"
    "rd.driver.pre=vfio-pci"
    "hugepagesz=1G"
    "default_hugepagesz=1G"
    "hugepages=25"
    "vfio-pci.ids=10de:1b06,10de:10ef,10de:1e02,10de:10f7,10de:1ad6,10de:1ad7,1912:0014"
    "vfio-pci.disable_vga=1"
    "kvm.ignore_msrs=1"
    "kvm.report_ignored_msrs=0"
    "kvm_amd.nested=0"
  ];
  boot.kernel.sysctl = {
    "fs.inotify.max_user_instances" = 8192;
    "fs.inotify.max_user_watches" = 1164444;
  };
  swapDevices = [ ];
  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  hardware.video.hidpi.enable = lib.mkDefault true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = with pkgs; [
    amdvlk
    driversi686Linux.amdvlk
    rocm-opencl-icd
    rocm-opencl-runtime
  ];

  boot.loader = {
    grub = {
      enable = false;
      default = "saved";
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };
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

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [];
  };

  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [ libguestfs ];

  environment.sessionVariables = {
    ### use emacsclient as default editor
    EDITOR = "emacsclient -t";
    VISUAL = "emacsclient -t";
    ### tell libraries to use wayland
    GDK_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland-egl";
    CLUTTER_BACKEND = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    ECORE_EVAS_ENGINE = "wayland-egl";
    ELM_ENGINE = "wayland_egl";
    SDL_VIDEODRIVER = "wayland";
    ### visual config
    FREETYPE_PROPERTIES = "truetype:interpreter-version=40";
    # FREETYPE_PROPERTIES = "truetype:interpreter-version=38";
    INFINALITY_FT = "osx";
    XCURSOR_THEME = "capitaine-cursors-white";
    XCURSOR_SIZE = "24";
    ### misc
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent.socket";
    LEIN_JVM_OPTS = "-Xms100m -Xmx300m";
    _GLOBAL_ENV_LOADED = "1";
  };

  services.pipewire = {
    enable = false;
    alsa.enable = true;
    pulse.enable = true;
  };

  # xdg-desktop-portal works by exposing a series of D-Bus interfaces
  # known as portals under a well-known name
  # (org.freedesktop.portal.Desktop) and object path
  # (/org/freedesktop/portal/desktop).
  # The portal interfaces include APIs for file access, opening URIs,
  # printing and others.
  services.dbus.enable = true;

  xdg.portal = {
    enable = true;
    wlr.enable = false;
    # gtk portal needed to make gtk apps happy
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    # gtkUsePortal = true;
  };

  system.stateVersion = "22.11";
}
