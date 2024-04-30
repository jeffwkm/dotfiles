{ config, lib, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt;
  inherit (config) user modules;
  inherit (modules) programs;
in {
  ## declare linux options needed for build
  options.modules.desktop.enable = mkBoolOpt false;
  options.modules.wayland.enable = mkBoolOpt false;

  config = {
    nix.settings.trusted-users = [ "@admin" ];
    nix.configureBuildUsers = true;
    environment.loginShell = pkgs.zsh;
    services.nix-daemon.enable = true;
    programs.mosh.enable = true;

    homebrew.brews = optionals programs.mpv.enable [ "mpv" "vapoursynth" ];
    homebrew.casks = with programs;
      optional spotify.enable "spotify"
      ++ optional vscode.enable "visual-studio-code"
      ++ optional firefox.enable "firefox"
      ++ optional alacritty.enable "alacritty";

    # Set of files to be linked in '/Library/LaunchAgents'
    environment.launchAgents = { };
    # Set of files to be linked in '/Library/LaunchDaemons'
    environment.launchDaemons = {
      # ./com.jeff.pre-login.plist
    };
    # Set of files to be linked in '~/Library/LaunchDaemons'
    environment.userLaunchAgents = { };

    environment.etc."pf.conf" = mkIf false (readFile ./pf.conf);
    environment.etc."pf.anchors/port80" =
      mkIf false (readFile ./pf.anchors/port80);

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ (lowPrio coreutils-full) ];
    };
  };
}
