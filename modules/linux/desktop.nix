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
    gnome = mkBoolOpt true;
    qt = mkBoolOpt true;
    amdgpu-fan = mkBoolOpt false;
    steam = mkBoolOpt (pkgs.system != "aarch64-linux");
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = optional cfg.gnome (final: prev: {
      gnome = prev.gnome.overrideScope (gfinal: gprev: {
        nautilus = gprev.nautilus.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ (with prev.gst_all_1; [
            gst-plugins-good
            gst-plugins-bad
            gst-plugins-ugly
            gst-libav
            prev.ffmpegthumbnailer
          ]);
        });
      });
    });

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
    programs.gnome-disks.enable = cfg.gnome;

    services.gvfs.enable = true;
    # services.tumbler.enable = true; # thumbnailer

    # programs.thunar.enable = true; # xfce file manager
    # programs.xfconf.enable = true; # xfce settings

    services.gnome = mkIf cfg.gnome {
      tracker.enable = true;
      tracker-miners.enable = true;
      sushi.enable = true;
    }

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
      # xdgOpenUsePortal = true;
      extraPortals = with pkgs;
        [ xdg-desktop-portal-gtk ]
        ++ optional wayland.enable xdg-desktop-portal-wlr
        ++ optional wayland.hyprland.enable xdg-desktop-portal-hyprland;
    };

    programs.steam.enable = cfg.steam;

    environment.sessionVariables.GST_PLUGIN_SYSTEM_PATH_1_0 =
      lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (with pkgs.gst_all_1; [
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
      ]);

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        gnomePackages = with pkgs;
          optionals cfg.gnome ([
            baobab
            ffmpegthumbnailer
            gtk3
            gtk4
            themechanger
            gnome-podcasts
            gnome-usage
            gedit
            gobject-introspection
            gthumb
          ] ++(with gnome; [
            dconf-editor
            eog
            evince
            file-roller
            nautilus
            totem
          ]));
        qtPackages = with pkgs;
          optionals cfg.qt [ qt5.full qt5.qtwayland qt5ct adwaita-qt ];
        cli = with pkgs;
          [
            ddcutil
            keyd
            latencytop
            libinput
            libnotify
            ncpamixer
            pamixer
            playerctl
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
