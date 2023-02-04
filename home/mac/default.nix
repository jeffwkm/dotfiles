{ config, lib, pkgs, ... }:
with pkgs;
let
  python3-packages = (python-packages:
    with python-packages; [
      beautifulsoup4
      pandas
      requests
      pip
      isort
      pytest
      mpv
      python-mpv-jsonipc
      pyflakes
      nose
      setuptools
      black
      jsbeautifier
      vapoursynth
    ]);
  python3-custom = python3.withPackages python3-packages;
in {
  imports = [ ../zsh/mac.nix ../gui/alacritty.nix ];

  home.sessionPath = [
    "${config.home.homeDirectory}/.rustup/toolchains/stable-aarch64-apple-darwin/bin"
  ];

  alacritty.fontSize = "15";
  alacritty.fontSizeLarge = "17";
  alacritty.fontSizeHuge = "19";
  alacritty.decorations = "buttonless";
  alacritty.fontFamily = "JetBrainsMono Nerd Font";
  alacritty.fontStyle = "SemiBold";

  xdg.configFile."mpv/mpv.conf".text =
    (builtins.readFile ../../dotfiles/mpv/mpv.conf) + ''
      icc-profile-auto=yes
      hwdec=auto
      input-ipc-server=/tmp/mpvsocket
    '';

  xdg.configFile."mpv/input.conf".text =
    (builtins.readFile ../../dotfiles/mpv/input.conf) + "";

  home.packages = with pkgs; [
    (lowPrio coreutils-full)
    cmake
    curl
    editorconfig-checker
    editorconfig-core-c
    fd
    font-awesome_5
    fontconfig
    gnugrep
    gnumake
    gnused
    lsd
    nerd-font-patcher
    nerdfonts
    nodejs
    openssh
    pandoc
    pass
    # python3-custom
    ranger
    rbenv
    ripgrep
    ripgrep-all
    rsync
    rustup
    rustracer
    shfmt
    todoist
    watch
    xz
  ];
}
