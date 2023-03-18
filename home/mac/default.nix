{ config, lib, pkgs, ... }:
with pkgs; {
  imports = [ ../zsh/mac.nix ../gui/alacritty.nix ];

  home.sessionPath = [
    "${config.home.homeDirectory}/.rustup/toolchains/stable-aarch64-apple-darwin/bin"
  ];

  alacritty.fontSize = "14";
  alacritty.fontSizeLarge = "16";
  alacritty.fontSizeHuge = "18";
  alacritty.decorations = "buttonless";
  alacritty.fontFamily = "JetBrainsMono Nerd Font";
  # alacritty.fontStyle = "SemiBold";

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
    curl
    fd
    font-awesome_5
    fontconfig
    gnugrep
    gnused
    lsd
    nerd-font-patcher
    nerdfonts
    openssh
    pass
    ranger
    rbenv
    ripgrep
    ripgrep-all
    rsync
    watch
    xz
  ];
}
