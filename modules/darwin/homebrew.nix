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
      "d12frosted/emacs-plus"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "koekeishiya/formulae"
      "railwaycat/emacsmacport"
      "cmacrae/formulae"
      "insilica/srvc"
    ];

    homebrew.brews = [
      "FelixKratz/formulae/sketchybar"
      "adwaita-icon-theme"
      "autoconf"
      "automake"
      "binutils"
      "c-ares"
      "cairo"
      "cmake"
      "coreutils"
      "expat"
      "ffmpeg"
      "gcc"
      "giflib"
      "glib"
      "gnutls"
      "gtk+3"
      "ifstat"
      # "insilica/srvc/srvc"
      "imagemagick"
      "ispell"
      "jansson"
      "jpeg"
      "libgccjit"
      "libheif"
      "libiconv"
      "libpng"
      "librsvg"
      "libxml2"
      "mailutils"
      "make"
      "pango"
      "pkg-config"
      "python3"
      "texinfo"
      "zlib"
      "zsh-completions"
    ];

    # homebrew.masApps = { Slack = 803453959; };

    homebrew.casks = [
      "daisydisk"
      "docker"
      "elgato-game-capture-hd"
      "google-chrome"
      "istat-menus"
      "iterm2"
      "pgadmin4"
      # "rescuetime"
      # "slack"
      "snes9x"
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
