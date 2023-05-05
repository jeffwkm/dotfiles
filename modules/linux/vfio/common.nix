{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.vfio;
in {
  options.modules.vfio = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      OVMF
      libvirt
      qemu
      qemu-utils
      scream
      virt-manager
    ];

    systemd.tmpfiles.rules = [
      "f /dev/shm/scream          0660 ${user.name} qemu-libvirtd -"
      "f /dev/shm/looking-glass   0660 ${user.name} qemu-libvirtd -"
    ];

    # (on libvirtd startup) set machine definitions from nix config
    systemd.services.vm-define = let
      vm-define-sh = pkgs.writeShellScript "vm-define.sh" ''
        [ -d /etc/machines ] || exit 0
        for x in /etc/machines/*.xml ; do
          ${pkgs.libvirt}/bin/virsh define "$x" --validate
        done
      '';
    in {
      enable = true;
      after = [ "libvirtd.service" ];
      requires = [ "libvirtd.service" ];
      before = [ "libvirt-guests.service" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${vm-define-sh}";
      };
    };

    systemd.user.services.scream = {
      enable = true;
      description = "Scream IVSHMEM";
      serviceConfig = {
        Type = "exec";
        ExecCondition = ''
          ${pkgs.bash}/bin/bash -c '[ "$(sudo virsh -E domstate win10)" == "running" ]' '';
        ExecStart =
          "${pkgs.scream}/bin/scream -m /dev/shm/scream -o pulse -t 16 -v";
        Restart = "always";
        RestartSec = 60;
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
      onBoot = "start";
      onShutdown = "shutdown";
      qemu = {
        ovmf.enable = true;
        runAsRoot = true;
        swtpm.enable = true;
      };
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: { };
  };
}
