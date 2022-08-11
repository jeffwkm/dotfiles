{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.programs.skhd;
in {
  options.modules.programs.skhd = { enable = mkBoolOpt true; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ skhd ];

    services.skhd = {
      enable = true;
      package = pkgs.skhd;
      skhdConfig = (import ./_skhdrc.nix {
        # yabai = "${pkgs.yabai}/bin/yabai";
        yabai = "/opt/homebrew/bin/yabai";
        apps = {
          editor = "Emacs";
          browser = "Safari";
          terminal = "Alacritty";
          music = "Spotify";
          video = "mpv";
        };
        keys = {
          prev = "0x29"; # ";"
          prevAlt = "0x2B"; # ","
          next = "0x27"; # "'"
          nextAlt = "0x2F"; # "."
          first = "0x21"; # "["
          last = "0x1E"; # "]"
          recent = "space";
          left = "left";
          right = "right";
          up = "up";
          down = "down";
          sticky = "s";
          top = "o";
          stack = "q";
          float = "w";
          bsp = "e";
        };
        modifiers = {
          focus = "alt";
          move = "cmd + shift";
          layout = "alt + ctrl";
          resize = "cmd + ctrl + shift";
          display = "alt + shift";
          launch = "cmd + ctrl";
          misc = "alt";
        };
        appKeys = {
          editor = "e";
          browser = "b";
          terminal = "return";
          music = "m";
          video = "v";
        };
        commands = {
          launchTerminal = "~/bin/alacritty-open.sh";
          activateApp = "~/bin/activate-app.sh";
        };
        extra = "";
      }).cfg;
    };
  };
}
