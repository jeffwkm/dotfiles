{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.programs.yabai;
in {
  options.modules.programs.yabai = {
    enable = mkBoolOpt false;
    service = mkBoolOpt cfg.enable;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        yabai = prev.yabai.overrideAttrs (old: {
          src = builtins.fetchGit {
            url = "https://github.com/koekeishiya/yabai.git";
            ref = "master";
            rev = "d5242378031e7b66c28c62775f05f22755fd1583";
          };
        });
      })
    ];

    environment.systemPackages = with pkgs; [ yabai ];

    services.yabai = mkIf cfg.service {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = true;

      config = {
        mouse_follows_focus = "off";
        focus_follows_mouse = "autofocus"; # off autofocus autoraise

        layout = "stack"; # bsp stack float
        window_topmost = "off";
        window_placement = "second_child";
        split_ratio = 0.5;
        auto_balance = "on";
        window_opacity = "off";
        # window_opacity_duration = 0.0;
        active_window_opacity = 1.0;
        normal_window_opacity = 0.9;
        window_shadow = "off";

        mouse_modifier = "alt";
        mouse_action1 = "move";
        mouse_action2 = "resize";

        top_padding = 6;
        bottom_padding = 6;
        left_padding = 6;
        right_padding = 6;
        window_gap = 6;

        window_border = "off";
        # window_border_placement = "inset";
        # window_border_width = 4;
        # window_border_radius = -1.0;
        # active_window_border_topmost = "off";
        # active_window_border_color = "0xff775759";
        # normal_window_border_color = "0xff505050";
        # insert_window_border_color = "0xffd75f5f";
      };

      extraConfig = ''
        yabai -m rule --add label=prefs   app="System Preferences"   manage=off
        yabai -m rule --add label=svp     app="SVP 4 Mac"            manage=off
        yabai -m rule --add label=steam   app="Steam"                manage=off
        yabai -m rule --add label=mpv     app="mpv"                  manage=off
      '';
    };
  };
}
