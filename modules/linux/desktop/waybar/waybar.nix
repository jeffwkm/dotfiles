{ config, lib, pkgs, ... }:
with builtins;
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland.waybar;
  pwd = "${host.config-dir}/modules/linux/desktop/waybar";
in {
  options.modules.wayland.waybar = {
    enable = mkBoolOpt modules.wayland.sway.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [ (final: prev: { waybar = optimize config prev.waybar; }) ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = config.lib.file.mkOutOfStoreSymlink;
      in {
        xdg.configFile."waybar/config".source = link "${pwd}/config";

        xdg.configFile."waybar/style.css".source = link "${pwd}/style.css";

        systemd.user.services.waybar = {
          Unit = {
            Description = "Wayland bar for Sway and Wlroots based compositors";
            PartOf = [ "graphical-session.target" ];
          };
          Service = {
            Type = "simple";
            ExecCondition =
              ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
            ExecStart = "${pkgs.waybar}/bin/waybar";
          };
        };

        home.packages = [ pkgs.waybar ];
      };
  };
}
