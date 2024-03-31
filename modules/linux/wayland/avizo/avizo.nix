{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = modules.wayland.avizo;
in {
  options.modules.wayland.avizo = {
    enable = mkBoolOpt' modules.wayland.enable
      "Enable avizo (MacOS-style volume/brightness OSD)";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { avizo = optimize config prev.avizo; }) ];

    environment.systemPackages = with pkgs; [ avizo brightnessctl ddcutil ];

    # need "ddcci" kernel module for brightnessctl
    boot.extraModulePackages = with config.boot.kernelPackages;
      [ ddcci-driver ];

    boot.kernelModules = [ "i2c-dev" "ddcci_backlight" ];

    services.udev.extraRules = ''
      SUBSYSTEM=="i2c-dev", ACTION=="add",\
        ATTR{name}=="AMDGPU*",\
        TAG+="ddcci",\
        TAG+="systemd",\
        ENV{SYSTEMD_WANTS}+="ddcci@$kernel.service"
    '';

    systemd.services."ddcci@" = {
      scriptArgs = "%i";
      script = ''
        echo Trying to attach ddcci to $1
        id=$(echo $1 | cut -d "-" -f 2)
        counter=10
        while [ $counter -gt 0 ]; do
          if ${pkgs.ddcutil}/bin/ddcutil getvcp 10 -b $id; then
            echo ddcci 0x37 > /sys/bus/i2c/devices/$1/new_device
            break
          fi
          sleep 2
          counter=$((counter - 1))
        done
      '';
      serviceConfig.Type = "oneshot";
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      xdg.configFile."avizo/config.ini".source =
        config.lib.file.mkOutOfStoreSymlink
        "${host.config-dir}/modules/linux/wayland/avizo/config.ini";

      systemd.user.services.avizo = {
        Unit = {
          Description = "avizo-service (volume/brightness OSD for Wayland)";
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.avizo}/bin/avizo-service";
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
  };
}
