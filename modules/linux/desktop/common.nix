{ config, lib, pkgs, ... }:
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
    gnome.enable = mkBoolOpt cfg.enable;
    gnome.services.enable = mkBoolOpt false;
    qt = mkBoolOpt false;
    amdgpu-fan = mkBoolOpt false;
    steam = mkBoolOpt (pkgs.system != "aarch64-linux");
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = optional cfg.gnome.enable (final: prev: {
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
      };
    };

    xdg.mime.enable = true;

    console = {
      earlySetup = false;
      keyMap = "us";
      packages = with pkgs; [ terminus_font ];
      font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    };

    services.logind.extraConfig = ''
      # don’t shutdown when power button is short-pressed
      HandlePowerKey=ignore
    '';

    users.extraUsers.root.extraGroups =
      [ "audio" "input" "plugdev" "libvirtd" ];

    sound.enable = true;
    programs.dconf.enable = true;
    security.rtkit.enable = true;
    security.polkit.enable = true;
    services.gvfs.enable = true;
    services.udisks2.enable = true;
    programs.gnome-disks.enable = cfg.gnome.enable;

    services.gnome = mkIf cfg.gnome.services.enable {
      tracker.enable = true;
      tracker-miners.enable = true;
      sushi.enable = true;
    };

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

    programs.steam = {
      enable = cfg.steam;
      # remotePlay.openFirewall = true;
      # dedicatedServer.openFirewall = true;
      gamescopeSession.enable = true;
      # gamescopeSession.package = pkgs.gamescope_git;
    };

    environment.sessionVariables.GST_PLUGIN_SYSTEM_PATH_1_0 =
      lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (with pkgs.gst_all_1; [
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
      ]);

    environment.sessionVariables.XCURSOR_THEME = "capitaine-cursors-white";
    environment.sessionVariables.XCURSOR_SIZE = "24";
    environment.sessionVariables.FREETYPE_PROPERTIES =
      "truetype:interpreter-version=38";

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        gnomePackages = with pkgs;
          optionals cfg.gnome.enable ([
            baobab
            ffmpegthumbnailer
            gedit
            glib
            gjs
            gnome-podcasts
            gnome-usage
            gobject-introspection
            gradience
            gthumb
            gtk3
            gtk4
            gtkperf
            libadwaita
            libpulseaudio
            themechanger
          ] ++ (with gnome; [
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
            cava
            ddcutil
            keyd
            latencytop
            libinput
            libnotify
            ncpamixer
            pamixer
            playerctl
            slack-term
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
        x11 = with pkgs; [
          mesa-demos
          xorg.xev
          xorg.xkbcomp
          xorg.xkill
          xorg.xprop
          xorg.xrandr
          xorg.xrdb
          xorg.xset
          xorg.xwininfo
          xterm
        ];
        themes = with pkgs;
          [ vivid base16-builder base16-shell-preview ] ++ [
            adw-gtk3
            capitaine-cursors
            catppuccin-cursors
            # papirus-icon-theme
            catppuccin-papirus-folders
            catppuccin-sddm-corners
            gnome.adwaita-icon-theme
            gnome-icon-theme
            nordic
          ] ++ optionals modules.desktop.qt
          [ plasma5Packages.qtstyleplugin-kvantum ];
        catppuccin-mocha = (pkgs.catppuccin-gtk.override {
          variant = "mocha";
          size = "compact";
          tweaks = [ "rimless" ];
        });
        catppuccin-macchiato = (pkgs.catppuccin-gtk.override {
          variant = "macchiato";
          size = "compact";
          tweaks = [ "normal" ];
        });
        catppuccin = [ pkgs.catppuccin catppuccin-mocha catppuccin-macchiato ];
      in {
        home.packages = gnomePackages ++ qtPackages ++ cli ++ gui ++ themes
          ++ x11 ++ catppuccin;

        services.easyeffects = { enable = true; };
        services.pass-secret-service.enable = true;
        # services.gnome-keyring.enable = true;
        # services.gnome-keyring.components = [ "secrets" ];
        services.udiskie.enable = true;

        gtk = {
          enable = true;
          font.name = "sans";
          font.size = 11;
          cursorTheme.name = "capitaine-cursors-white";
          cursorTheme.package = pkgs.capitaine-cursors;
          cursorTheme.size = 24;
          iconTheme.name = "Papirus";
          iconTheme.package = pkgs.catppuccin-papirus-folders;
          theme.name = "Catppuccin-Macchiato-Compact-Blue-Dark";
          theme.package = catppuccin-macchiato;

          # theme.package = pkgs.adw-gtk3;

          ## validVariants = [ "latte" "frappe" "macchiato" "mocha" ];
          ## validAccents = [ "blue" "flamingo" "green" "lavender" "maroon" "mauve" "peach" "pink" "red" "rosewater" "sapphire" "sky" "teal" "yellow" ];
          ## validSizes = [ "standard" "compact" ];
          ## validTweaks = [ "black" "rimless" "normal" ];

          gtk3.bookmarks = [
            "file:///home/${user.name}/Downloads"
            "file:///home/jeff/code"
            "file:///mnt/huge/TV%20Shows"
            "file:///mnt/huge/Movies"
            "file:///mnt/huge/Torrents"
            "file:///mnt/huge/Youtube"
            "file:///mnt/huge/files"
          ];
          gtk3.extraConfig = { gtk-application-prefer-dark-theme = true; };
          gtk4.extraConfig = { gtk-application-prefer-dark-theme = true; };
        };

        home.file.".Xresources.d".source = ./Xresources.d;
        home.file.".Xresources".source =
          ./Xresources.d/.Xresources.TomorrowNight;
        xdg.configFile."cava/config".source = ./cava/config;

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
