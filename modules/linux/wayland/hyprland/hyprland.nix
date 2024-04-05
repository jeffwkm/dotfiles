{ config, lib, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = modules.wayland.hyprland;
  pwd = "${host.config-dir}/modules/linux/wayland/hyprland";
  optimize' = optimize config;
in {
  options.modules.wayland.hyprland = {
    enable = mkBoolOpt modules.wayland.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      inputs.hyprland.overlays.default
      inputs.hyprland.overlays.wlroots-hyprland
      (final: prev: {
        wlroots-hyprland = optimize' prev.wlroots-hyprland;
        hyprland-unwrapped = optimize' (prev.hyprland-unwrapped.override {
          wlroots = final.wlroots-hyprland;
        });
        hyprland = optimize'
          (prev.hyprland.override { wlroots = final.wlroots-hyprland; });
      })
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      imports = [ inputs.hyprland.homeManagerModules.default ];

      home.packages = with pkgs; [
        hypridle
        hyprlock
        hyprpaper
        hyprland-autoname-workspaces
      ];

      xdg.configFile."hypr/hyprland.conf".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/hyprland.conf";

      xdg.configFile."hyprland-autoname-workspaces/config.toml".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/_autoname_config.toml";

      wayland.windowManager.hyprland = {
        enable = true;
        systemd.enable = false;
        # extraConfig = builtins.readFile ./hyprland.conf;
      };

      systemd.user.targets.hyprland-session = {
        Unit = {
          Description = "Hyprland compositor session";
          BindsTo = [ "graphical-session.target" ];
        };
      };

      systemd.user.services.mako.Install.WantedBy =
        mkIf modules.wayland.mako.enable [ "hyprland-session.target" ];

      # systemd.user.services.waybar.Install.WantedBy =
      #   mkIf modules.wayland.waybar.enable [ "hyprland-session.target" ];

      systemd.user.services.ags.Install.WantedBy =
        mkIf modules.wayland.ags.enable [ "hyprland-session.target" ];

      systemd.user.services.avizo.Install.WantedBy =
        mkIf modules.wayland.avizo.enable [ "hyprland-session.target" ];

      systemd.user.services.lxqt-policykit-agent.Install.WantedBy =
        mkIf modules.wayland.enable [ "hyprland-session.target" ];

      systemd.user.services.hypridle = {
        Unit = {
          Description = "Idle manager for Hyprland compositor";
          PartOf = [ "graphical-session.target" ];
        };
        Install = { WantedBy = [ "hyprland-session.target" ]; };
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.hypridle}/bin/hypridle";
          Restart = "always";
          RestartSec = 5;
        };
      };

      systemd.user.services.hyprland-autoname-workspaces = {
        Unit = {
          Description = "hyprland-autoname-workspaces service";
          PartOf = [ "graphical-session.target" ];
        };
        Install = { WantedBy = [ "hyprland-session.target" ]; };
        Service = {
          Type = "simple";
          ExecStart =
            "${pkgs.hyprland-autoname-workspaces}/bin/hyprland-autoname-workspaces";
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
  };
}
