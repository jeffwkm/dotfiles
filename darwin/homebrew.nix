{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf;
  mkIfCaskPresent = cask: mkIf (lib.any (x: x == cask) config.homebrew.casks);
  brewEnabled = config.homebrew.enable;
in {
  environment.shellInit = mkIf brewEnabled ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.onActivation.upgrade = true;
  homebrew.brewPrefix = "/opt/homebrew/bin";
  homebrew.cleanup = "zap"; # "none" "uninstall" "zap"
  homebrew.global.brewfile = true;
  homebrew.global.lockfiles = false;

  homebrew.taps = [
    "FelixKratz/formulae"
    # "d12frosted/emacs-plus"
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
    "aspell"
    "autoconf"
    "automake"
    "binutils"
    "c-ares"
    "cairo"
    "cmake"
    "coreutils"
    "curl"
    "entr"
    "expat"
    "fd"
    "ffmpeg"
    "gcc"
    "gh"
    "giflib"
    "git"
    "glib"
    "gtk+3"
    "ifstat"
    "insilica/srvc/srvc"
    "imagemagick"
    "ispell"
    "jpeg"
    "libgccjit"
    "libheif"
    "libiconv"
    "libpng"
    "librsvg"
    "libxml2"
    "lsd"
    "mailutils"
    "make"
    "mpv"
    "pango"
    "pass"
    "pkg-config"
    "python3"
    "rbenv"
    "subversion"
    "tmux"
    "vapoursynth"
    "yt-dlp"
    "zsh"
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
    "rescuetime"
    # "slack"
    # "snes9x"
    "sf-symbols"
    "spotify"
    "steam"
    "visual-studio-code"
    "vlc"
  ];

  homebrew.extraConfig = ''
      brew "railwaycat/emacsmacport/emacs-mac", args: ["with-native-comp", "with-emacs-big-sur-icon", "with-mac-metal"]
      # brew "d12frosted/emacs-plus/emacs-plus@28", args: ["build-from-source", "with-native-comp", "with-modern-paper-icon", "with-no-frame-refocus"]
      brew "koekeishiya/formulae/yabai", args: ["HEAD"]
      brew "FelixKratz/formulae/fyabai", args: ["HEAD"]
      brew "nginx", restart_service: true
      brew "md5sha1sum", link: false
    '';
}
