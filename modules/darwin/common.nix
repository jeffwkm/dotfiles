{ config, lib, pkgs, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt;
  inherit (config) user modules;
  inherit (modules) programs;
in {
  ## declare linux options needed for build
  options.modules.desktop.enable = mkBoolOpt false;

  config = {
    host.gui = true;
    nix.enable = false;

    environment.shellInit = mkIf config.homebrew.enable ''
      eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
    '';

    homebrew.enable = true;
    homebrew.onActivation.autoUpdate = true;
    homebrew.onActivation.cleanup = "zap"; # "none" "uninstall" "zap"
    homebrew.onActivation.upgrade = true;
    homebrew.brewPrefix = "/opt/homebrew/bin";
    homebrew.global.brewfile = true;
    homebrew.global.lockfiles = false;

    homebrew.taps = [
      "FelixKratz/formulae"
      # "d12frosted/emacs-plus"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "koekeishiya/formulae"
      # "railwaycat/emacsmacport"
      "cmacrae/formulae"
    ];

    homebrew.brews = [ "FelixKratz/formulae/sketchybar" ]
      ++ [ "python3" "virtualenv" "poetry" ]
      ++ optionals programs.mpv.enable [ "mpv" "vapoursynth" "ffmpeg" ];

    # homebrew.masApps = { Slack = 803453959; };

    homebrew.casks = [
      "alfred"
      "daisydisk"
      "docker"
      "istat-menus"
      "proton-mail-bridge"
      "sf-symbols"
      "steam"
      "superwhisper"
      "vlc"
    ] ++ (with programs;
      optional spotify.enable "spotify"
      ++ optional vscode.enable "visual-studio-code"
      ++ optional chromium.enable "google-chrome"
      ++ optional firefox.enable "firefox"
      ++ optional alacritty.enable "alacritty"
      ++ optional kitty.enable "kitty");

    homebrew.extraConfig = ''
      brew "koekeishiya/formulae/yabai", args: ["HEAD"]
      brew "nginx", restart_service: true
      brew "md5sha1sum", link: false
    '';

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
