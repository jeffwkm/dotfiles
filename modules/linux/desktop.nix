{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host;
  inherit (host) darwin;
  cfg = config.modules.desktop;
  amdgpu-fan = pkgs.python3Packages.callPackage ./_amdgpu-fan.nix { };
in {
  options.modules.desktop = {
    enable = mkBoolOpt false;
    gnome = { enable = mkBoolOpt cfg.enable; };
    qt.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ amdgpu-fan ];

    i18n.defaultLocale = "en_US.UTF-8";
    time.timeZone = "America/New_York";

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
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    environment.etc."pipewire/pipewire-pulse.d/92-low-latency.conf".source =
      let json = pkgs.formats.json { };
      in json.generate "92-low-latency.conf" {
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

    xdg.portal = {
      enable = true;
      # wlr.enable = modules.wayland.enable;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      # gtkUsePortal = true;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        gnomePackages = with pkgs;
          optionals cfg.gnome.enable [
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
            gnome.gedit
            gnome.nautilus
            gnome.sushi
            gobject-introspection
            gthumb
            polkit_gnome
          ];
        qtPackages = with pkgs;
          optionals cfg.qt.enable [ qt5.full qt5.qtwayland ];
        cli = with pkgs;
          [
            xdg-user-dirs
            usbutils
            libnotify
            pamixer
            playerctl
            latencytop
            libinput
            ncpamixer
            pamixer
            playerctl
            ddcutil
            ffmpegthumbnailer
            caerbannog
            podgrab
          ] ++ [
            smartmontools
            lm_sensors
            acpi
            exfat
            kernelshark
            pciutils
            linuxPackages.cpupower
            glib
            exfat
          ];
        gui = with pkgs; [ pinentry-gtk2 pavucontrol snes9x-gtk ];
      in {
        home.packages = gnomePackages ++ qtPackages ++ cli ++ gui;

        services.easyeffects = { enable = true; };

        services.pass-secret-service.enable = true;

        qt = {
          enable = true;
        } // mkIf cfg.gnome.enable {
          platformTheme = "gnome";
          style.name = "adwaita-dark";
          style.package = pkgs.adwaita-qt;
        };
      };
  };
}
