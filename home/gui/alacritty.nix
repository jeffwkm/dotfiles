{ config, lib, pkgs, ... }: {
  options = {
    alacritty.fontFamily = lib.mkOption {
      type = lib.types.str;
      default = "JetBrains Mono Nerd Font";
    };
    alacritty.fontStyle = lib.mkOption {
      type = lib.types.str;
      default = "Medium";
    };
    alacritty.opacity = lib.mkOption {
      type = lib.types.str;
      default = "0.85";
    };
    alacritty.padding = lib.mkOption {
      type = lib.types.str;
      default = "4";
    };
    alacritty.offset.x = lib.mkOption {
      type = lib.types.str;
      default = "0";
    };
    alacritty.offset.y = lib.mkOption {
      type = lib.types.str;
      default = "0";
    };
    alacritty.fontSize = lib.mkOption {
      type = lib.types.str;
      default = "12";
      description = "Alacritty font size (standard)";
    };
    alacritty.fontSizeLarge = lib.mkOption {
      type = lib.types.str;
      default = "13";
      description = "Alacritty font size (large)";
    };
    alacritty.fontSizeHuge = lib.mkOption {
      type = lib.types.str;
      default = "15";
      description = "Alacritty font size (huge)";
    };
    alacritty.decorations = lib.mkOption {
      type = lib.types.str;
      default = "full";
    };
  };
  config = let
    tmpl = ./alacritty.yml.nix;
    options = config.alacritty;
    customize = c: lib.recursiveUpdate options c;
  in {
    xdg.configFile."alacritty/alacritty.yml" = import tmpl options;
    xdg.configFile."alacritty/alacritty.large.yml" =
      import tmpl (customize { fontSize = options.fontSizeLarge; });
    xdg.configFile."alacritty/alacritty.huge.yml" =
      import tmpl (customize { fontSize = options.fontSizeHuge; });
  };
}
