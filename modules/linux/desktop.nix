{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) wayland;
  cfg = config.modules.desktop;
  amdgpu-fan = pkgs.python3Packages.callPackage ./_amdgpu-fan.nix { };
in {
  options.modules.desktop = {
    enable = mkBoolOpt false;
    gnome = mkBoolOpt cfg.enable;
    qt = mkBoolOpt false;
    amdgpu-fan = mkBoolOpt false;
    steam = mkBoolOpt (pkgs.system != "aarch64-linux");
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ gparted ] ++ optional cfg.amdgpu-fan amdgpu-fan;

    systemd.services.amdgpu-fan = mkIf cfg.amdgpu-fan {
      enable = true;
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${amdgpu-fan}/bin/amdgpu-fan";
        Restart = "always";
        RestartSec = 60;
      };
    };

    console = {
      earlySetup = false;
      keyMap = "us";
      packages = with pkgs; [ terminus_font ];
      font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    };

    users.extraUsers.root.extraGroups =
      [ "audio" "input" "plugdev" "libvirtd" ];

    sound.enable = true;

    security.rtkit.enable = true;
    security.polkit.enable = true;

    services.udisks2 = { enable = true; };
    programs.gnome-disks.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = mkDefault true;
      pulse.enable = true;
      extraConfig.pipewire."92-low-latency.conf" = {
        context.modules = [{
          name = "libpipewire-module-protocol-pulse";
          args = {
            pulse.min.req = "32/48000";
            pulse.default.req = "32/48000";
            pulse.max.req = "32/48000";
            pulse.min.quantum = "32/48000";
            pulse.max.quantum = "32/48000";
          };
        }];
        stream.properties = {
          node.latency = "32/48000";
          resample.quality = 1;
        };
      };
    };

    xdg.portal = {
      enable = true;
      wlr.enable = wayland.enable;
      xdgOpenUsePortal = true;
      extraPortals = with pkgs;
        [ xdg-desktop-portal-gtk ]
        ++ optional wayland.enable xdg-desktop-portal-wlr
        ++ optional wayland.hyprland.enable xdg-desktop-portal-hyprland;
    };

    programs.steam.enable = cfg.steam;

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        gnomePackages = with pkgs;
          optionals cfg.gnome [
            baobab
            gtk3
            gtkperf
            lxappearance-gtk2
            gnome-podcasts
            gnome-usage
            gnome.dconf-editor
            gnome.eog
            gnome.evince
            gnome.file-roller
            gedit
            gnome.nautilus
            gnome.sushi
            gobject-introspection
            gthumb
          ];
        qtPackages = with pkgs; optionals cfg.qt [ qt5.full qt5.qtwayland ];
        cli = with pkgs;
          [
            caerbannog
            ddcutil
            ffmpegthumbnailer
            keyd
            latencytop
            libinput
            libnotify
            ncpamixer
            pamixer
            playerctl
            podgrab
            usbutils
            xdg-user-dirs
          ] ++ [
            acpi
            exfat
            glib
            kernelshark
            linuxPackages.cpupower
            lm_sensors
            pciutils
            smartmontools
          ];
        gui = with pkgs;
          [ pinentry-rofi pavucontrol ]
          ++ optionals cfg.steam [ steamcmd steam-tui ];
      in {
        home.packages = gnomePackages ++ qtPackages ++ cli ++ gui;

        services.easyeffects = { enable = true; };

        services.pass-secret-service.enable = true;

        services.udiskie.enable = true;

        qt = {
          enable = true;
        } // mkIf cfg.gnome {
          platformTheme = "gnome";
          style.name = "adwaita-dark";
          style.package = pkgs.adwaita-qt;
        };
      };
  };
}
