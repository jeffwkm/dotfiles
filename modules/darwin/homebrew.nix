{ config, lib, ... }:
with lib;
with lib.my; {
  config = {
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

    homebrew.brews = [ "FelixKratz/formulae/sketchybar" "python3" ];

    # homebrew.masApps = { Slack = 803453959; };

    homebrew.casks = [
      "daisydisk"
      "docker"
      "firefox"
      "google-chrome"
      "istat-menus"
      # "iterm2"
      "sf-symbols"
      "steam"
      "vlc"
    ];

    homebrew.extraConfig = ''
      brew "koekeishiya/formulae/yabai", args: ["HEAD"]
      brew "nginx", restart_service: true
      brew "md5sha1sum", link: false
    '';
  };
}
