{ config, lib, ... }: {
  environment.shellInit = lib.mkIf config.homebrew.enable ''
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
    "mpv"
    "pango"
    "pkg-config"
    "python3"
    "texinfo"
    "vapoursynth"
    "zlib"
    "zsh-completions"
  ];

  # homebrew.masApps = {
  #   Slack = 803453959;
  # };

  homebrew.casks = [
    "alacritty"
    "daisydisk"
    "docker"
    "elgato-game-capture-hd"
    "firefox"
    "google-chrome"
    "istat-menus"
    "iterm2"
    "pgadmin4"
    # "rescuetime"
    # "slack"
    "snes9x"
    "sf-symbols"
    "spotify"
    "steam"
    "visual-studio-code"
    "vlc"
  ];

  homebrew.extraConfig = ''
    brew "railwaycat/emacsmacport/emacs-mac", args: ["with-native-compilation", "with-emacs-big-sur-icon", "with-librsvg", "with-no-title-bars", "with-mac-metal" ]
    brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-native-comp", "with-modern-paper-icon", "with-no-titlebar-and-round-corners"]
    # brew "d12frosted/emacs-plus/emacs-plus@29", args: ["with-native-comp", "with-poll", "with-modern-paper-icon", "with-no-titlebar-and-round-corners", "with-no-frame-refocus"]
    # brew "d12frosted/emacs-plus/emacs-plus@30", args: ["with-native-comp", "with-modern-paper-icon", "with-no-frame-refocus", "with-poll"]
    brew "koekeishiya/formulae/yabai", args: ["HEAD"]
    brew "nginx", restart_service: true
    brew "md5sha1sum", link: false
  '';
}
