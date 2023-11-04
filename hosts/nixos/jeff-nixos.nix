{ config, options, pkgs, lib, modulesPath, ... }: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  config = {
    host.optimize = true;
    modules = {
      linux.systemd-boot = true;
      desktop.enable = true;
      emacs.enable = true;
      dev.enable-all = true;
      vfio.enable = true;
      wayland.enable = true;
      wayland.gammastep.enable = true;
      programs.alacritty.enable = true;
      programs.alacritty.fontStyle = "Semibold";
      programs.mpv.enable = true;
      programs.spotify.enable = true;
      programs.spotify.spotifyd.enable = true;
      programs.chromium.enable = true;
      services.mpd.enable = true;
      services.mpd.musicDirectory = "/mnt/huge/Music";
      programs.vscode.enable = true;
    };

    # services.printing.enable = true;

    environment.systemPackages = with pkgs; [
      firmwareLinuxNonfree
      libguestfs
      darktable
      # pgadmin
    ];

    # create machine definitions in /etc/machines
    environment.etc = { "machines/win10.xml".source = ./libvirt/win10.xml; };

    # create virtual proxy devices for usb devices passed to qemu
    # - this is needed so that qemu "-object input-linux,evdev=..." won't break
    #   if the host device is temporarily disconnected
    services.persistent-evdev = {
      enable = true;
      devices = {
        persist-keyboard0 = "usb-Topre_Corporation_HHKB_Professional-event-kbd";
        persist-mouse0 =
          "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-event-mouse";
        persist-mouse1 =
          "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-event-if02";
        persist-mouse2 =
          "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-if01-event-kbd";
        persist-steam0 = "usb-Valve_Software_Steam_Controller-event-mouse";
      };
    };

    virtualisation.libvirtd.qemu.verbatimConfig = ''
      cgroup_device_acl = [
        "/dev/null",
        "/dev/full",
        "/dev/zero",
        "/dev/random",
        "/dev/urandom",
        "/dev/ptmx",
        "/dev/kvm",
        "/dev/kqemu",
        "/dev/rtc",
        "/dev/hpet",
        "/dev/tpm0",
        "/dev/input/by-id/uinput-persist-keyboard0",
        "/dev/input/by-id/uinput-persist-mouse0",
        "/dev/input/by-id/uinput-persist-mouse1",
        "/dev/input/by-id/uinput-persist-mouse2",
        "/dev/input/by-id/uinput-persist-steam0"
      ]

      namespaces = []'';

    networking.interfaces.enp6s0.useDHCP = true;
    networking.bridges.br0.interfaces = [ "enp6s0" ];
    networking.dhcpcd.enable = true;
    networking.dhcpcd.allowInterfaces = [ "br0" ];
    systemd.services.br0-netdev.wantedBy = [ "multi-user.target" ];

    systemd.services.keyd = {
      enable = false;
      after = [ "systemd-udevd.service" "persistent-evdev.service" ];
      requires = [ "systemd-udevd.service" "persistent-evdev.service" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.keyd}/bin/keyd";
      };
    };

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
        neededForBoot = true;
        fsType = "ext4";
      };
      "/nix" = {
        device = "/mnt/arch/nix";
        depends = [ "/" "/mnt/arch" ];
        neededForBoot = true;
        fsType = "none";
        options = [ "bind" ];
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

    boot.kernelPackages = pkgs.linuxPackages_latest;

    boot.kernelModules = [ "kvm-amd" "i2c_dev" ];
    boot.initrd.availableKernelModules =
      [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
    # boot.initrd.kernelModules =
    #   [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" "amdgpu" ];
    boot.initrd.kernelModules = [
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
      "vfio_pci_core"
      "irqbypass"
      "iommufd"
      "amdgpu"
    ];
    boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
    boot.kernelParams = [
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
      "mitigations=off"
    ];

    hardware.cpu.amd.updateMicrocode = true;
    hardware.enableAllFirmware = true;
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        amdvlk
        rocm-opencl-icd
        rocm-opencl-runtime
        amdgpu_top
        vdpauinfo
        libva-utils
      ];
    };

    nix.settings.max-jobs = 32;
    nix.settings.cores = 16;

    system.stateVersion = "22.11";
  };
}
