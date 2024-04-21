{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
  inherit (config) user host modules;
  cfg = config.modules.programs.spotify;
in {
  options.modules.programs.spotify.enable = mkBoolOpt false;
  options.modules.programs.spotify.spotifyd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      imports = optionals modules.desktop.enable
        [ inputs.spicetify-nix.homeManagerModule ];

      home.packages = with pkgs; [ spotify-player sptlrx ];

      programs.spicetify = mkIf modules.desktop.enable {
        enable = true;

        # theme = spicePkgs.themes.text;
        # colorScheme = "CatppuccinMacchiato";

        theme = spicePkgs.themes.catppuccin;
        # colorScheme = "mocha";
        colorScheme = "macchiatto";

        enabledExtensions = with spicePkgs.extensions; [
          # fullAppDisplay
          shuffle # shuffle+ (special characters are sanitized out of ext names)
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
