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
    # boot.extraModulePackages = with config.boot.kernelPackages;
    #   [ ddcci-driver ];

    boot.kernelModules = [ "i2c-dev" ];

    systemd.services.ddcci = let
      loadMod = "${pkgs.kmod}/bin/modprobe ddcci";
      unloadMod = ''
        if (${pkgs.kmod}/bin/lsmod | ${pkgs.gnugrep}/bin/grep ddcci > /dev/null); then
          ${pkgs.kmod}/bin/rmmod ddcci_backlight ddcci || true;
          sleep 1.5 # wait to ensure hardware is ready for reload
        fi
      '';
      script = load: (unloadMod + optionalString load loadMod);
      ddcci-start = pkgs.writeShellScript "ddcci-start" (script true);
      ddcci-stop = pkgs.writeShellScript "ddcci-stop" (script false);
    in {
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${ddcci-start}";
        ExecStop = "${ddcci-stop}";
        RemainAfterExit = "yes";
      };
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
          ExecStart =
            "${pkgs.bash}/bin/bash -c '${pkgs.avizo}/bin/avizo-service | ${pkgs.gnugrep}/bin/grep -v Fade'";
          SyslogLevel = "debug";
          # LogRateLimitIntervalSec = "5s";
          # LogRateLimitBurst = "3";
          # LogFilterPatterns = "~.*Fade.*";
          # StandardError = "null";
          # StandardOutput = "null";
        };
      };
    };
  };
}
