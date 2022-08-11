{ config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    OVMF
    libvirt
    qemu
    qemu-utils
  ];

  systemd.tmpfiles.rules = [
    "f /dev/shm/looking-glass   0660 jeff qemu-libvirtd -"
    "f /dev/shm/scream          0660 jeff qemu-libvirtd -"
  ];

  # create machine definitions in /etc/machines
  environment.etc = {
    "machines/win10.xml".source = ./machines/win10.xml;
  };

  # (on libvirtd startup) set machine definitions from nix config
  systemd.services.vm-define = {
    enable = true;
    after = [ "libvirtd.service" ];
    requires = [ "libvirtd.service" ];
    before = [ "libvirt-guests.service" ];
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.libvirt}/bin/virsh define /etc/machines/win10.xml --validate";
    };
  };

  # create virtual proxy devices for usb devices passed to qemu
  # - this is needed so that qemu "-object input-linux,evdev=..." won't break
  #   if the host device is temporarily disconnected
  services.persistent-evdev = {
    enable = true;
    devices = {
      persist-keyboard0 = "usb-Topre_Corporation_HHKB_Professional-event-kbd";
      persist-mouse0 = "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-event-mouse";
      persist-mouse1 = "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-event-if02";
      persist-mouse2 = "usb-SteelSeries_SteelSeries_Prime_Mini_Wireless-if01-event-kbd";
      persist-steam0 = "usb-Valve_Software_Steam_Controller-event-mouse";
    };
  };

  systemd.user.services.scream = {
    enable = true;
    description = "Scream IVSHMEM";
    serviceConfig = {
      Type = "exec";
      ExecStart =
        "${pkgs.scream}/bin/scream -m /dev/shm/scream -o pulse -t 16 -v";
      Restart = "always";
      RestartSec = 3;
    };
    unitConfig = {
      StartLimitIntervalSec = 60;
      StartLimitBurst = 10;
    };
    wantedBy = [ "default.target" "pulseaudio.service" ];
    after = [ "pulseaudio.service" ];
    bindsTo = [ "pulseaudio.service" ];
  };

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      ovmf.enable = true;
      runAsRoot = true;
      swtpm.enable = true;
    };

    onBoot = "start";
    onShutdown = "shutdown";

    qemu.verbatimConfig = ''
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
  };
}
