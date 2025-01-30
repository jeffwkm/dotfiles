{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = modules.desktop.hyprland;
  pwd = "${host.config-dir}/modules/linux/desktop/hyprland";
  optimize' = optimizePkg {
    enable = host.optimize;
    level = 2;
    native = true;
  };
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt modules.desktop.enable;
    extraConf = mkOpt types.str "";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        hyprpaper =
          optimize' inputs.hyprpaper.packages.${pkgs.system}.hyprpaper;
      })
    ];

    programs.hyprland.enable = true;

    environment.systemPackages = with pkgs; [
      hypridle
      hyprlock
      hyprpaper
      hyprkeys
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = path: config.lib.file.mkOutOfStoreSymlink "${pwd}/${path}";
      in {
        xdg.configFile."hypr/hyprland.conf".source = link "hyprland.conf";
        xdg.configFile."hypr/hyprpaper.conf".source = link "hyprpaper.conf";
        xdg.configFile."hypr/hyprlock.conf".source = link "hyprlock.conf";
        xdg.configFile."hypr/mocha.conf".source = link "mocha.conf";
        xdg.configFile."hypr/hyprland.extra.conf".text = cfg.extraConf;

        systemd.user.targets.hyprland-session = {
          Unit = {
            Description = "Hyprland compositor session";
            BindsTo = [ "graphical-session.target" ];
          };
        };

        systemd.user.services.ags.Install.WantedBy =
          mkIf modules.desktop.ags.enable [ "hyprland-session.target" ];

        systemd.user.services.avizo.Install.WantedBy =
          mkIf modules.desktop.avizo.enable [ "hyprland-session.target" ];

        systemd.user.services.lxqt-policykit-agent.Install.WantedBy =
          mkIf modules.desktop.enable [ "hyprland-session.target" ];

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
            RestartSec = 60;
            SyslogLevel = "debug";
          };
        };

        systemd.user.services.hyprpaper = {
          Unit = {
            Description = "Wallpaper service for Hyprland";
            PartOf = [ "graphical-session.target" ];
          };
          Install = { WantedBy = [ "hyprland-session.target" ]; };
          Service = {
            Type = "simple";
            ExecStart = "${pkgs.hyprpaper}/bin/hyprpaper";
            SyslogLevel = "debug";
            LogLevelMax = "info";
            Restart = "always";
            RestartSec = 5;
          };
        };
      };
  };
}
