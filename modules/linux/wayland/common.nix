{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.wayland;
in {
  options.modules.wayland = { enable = mkBoolOpt modules.desktop.enable; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        pass = prev.pass-wayland;
      })
      (final: prev: {
        wl-clipboard-x11 = prev.stdenv.mkDerivation rec {
          pname = "wl-clipboard-x11";
          version = "5";

          src = prev.fetchFromGitHub {
            owner = "brunelli";
            repo = "wl-clipboard-x11";
            rev = "v${version}";
            sha256 = "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb";
          };

          dontBuild = true;
          dontConfigure = true;
          propagatedBuildInputs = [ prev.wl-clipboard ];
          makeFlags = [ "PREFIX=$(out)" ];
        };

        xsel = final.wl-clipboard-x11;
        xclip = final.wl-clipboard-x11;
      })
    ];

    environment.systemPackages = with pkgs; [ xsel xclip ];

    environment.sessionVariables = {
      GDK_BACKEND = "wayland";
      QT_QPA_PLATFORM = "wayland-egl";
      CLUTTER_BACKEND = "wayland";
      MOZ_ENABLE_WAYLAND = "1";
      ECORE_EVAS_ENGINE = "wayland-egl";
      ELM_ENGINE = "wayland_egl";
      SDL_VIDEODRIVER = "wayland";
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [
        grim
        egl-wayland
        swappy
        sway-contrib.grimshot
        wayland
        wayland-protocols
        wayland-utils
        wayshot
        wev
        wf-recorder
        wl-clipboard
        wlr-randr
        wtype
        sov
      ];
    };
  };
}
