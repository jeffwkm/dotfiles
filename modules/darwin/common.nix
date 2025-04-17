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

    environment.systemPackages = with pkgs; [
      pkg-config
      gobject-introspection
      glib
      gtk3
    ];

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
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "cmacrae/formulae"
      "koekeishiya/formulae"
      "FelixKratz/formulae"
    ];

    homebrew.brews =
      [ "FelixKratz/formulae/sketchybar" "python3" "virtualenv" "poetry" ]
      ++ [ "ruby" "gtk+3" "gobject-introspection" "adwaita-icon-theme" ]
      ++ (with programs; optional spotify.enable "spicetify-cli");
    # ++ optionals mpv.enable [ "vapoursynth" "ffmpeg" ]);

    # homebrew.masApps = { Slack = 803453959; };

    homebrew.casks = [
      "alfred"
      "daisydisk"
      "docker"
      "firefox"
      "istat-menus"
      "google-chrome"
      "macfuse"
      "proton-mail-bridge"
      "sf-symbols"
      "steam"
      "superwhisper"
      "vlc"
    ] ++ (with programs;
      optional spotify.enable "spotify"
      ++ optional vscode.enable "visual-studio-code"
      ++ optional kitty.enable "kitty"
      ++ optional alacritty.enable "alacritty");
    # ++ optional mpv.enable "mpv"

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

    # Fix linking of apps into Applications folder
    # https://github.com/nix-darwin/nix-darwin/issues/214#issuecomment-2467550779
    system.activationScripts.postUserActivation.text = ''
      apps_source="${config.system.build.applications}/Applications"
      moniker="Nix Trampolines"
      app_target_base="$HOME/Applications"
      app_target="$app_target_base/$moniker"
      mkdir -p "$app_target"
      ${pkgs.rsync}/bin/rsync --archive --checksum --chmod=-w --copy-unsafe-links --delete "$apps_source/" "$app_target"
    '';

    programs.nix-index.enable = true;

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ (lowPrio coreutils-full) ];
    };
  };
}
