{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
  gui = (modules.desktop.enable && pkgs.system != "aarch64-linux");
  cfg = config.modules.programs.spotify;
in {
  options.modules.programs.spotify = {
    enable = mkBoolOpt modules.desktop.enable;
    spotifyd.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      imports =
        optionals gui [ inputs.spicetify-nix.homeManagerModules.default ];

      home.packages = with pkgs; [ spotify-player sptlrx ];

      programs.spicetify = mkIf gui {
        enable = true;
        # theme = spicePkgs.themes.text;
        # colorScheme = "CatppuccinMacchiato";
        theme = spicePkgs.themes.catppuccin;
        # colorScheme = "mocha";
        colorScheme = "macchiatto";
        enabledExtensions = with spicePkgs.extensions; [
          shuffle
          hidePodcasts
          keyboardShortcut
          powerBar
        ];
      };

      services.spotifyd = mkIf cfg.spotifyd.enable {
        enable = true;
        package = pkgs.spotifyd.override {
          withPulseAudio = true;
          withMpris = true;
        };
        settings = {
          global = {
            username = "${user.email}";
            password_cmd = "pass show media/spotify";
            device_name = "${host.name}";
          };
        };
      };
    };
  };
}
