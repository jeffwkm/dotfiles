{ config, lib, pkgs, ... }:
let optimize = config.util.optimizeDefault;
in {
  imports = [ ./zsh ./dev.nix ];

  programs.gpg.enable = true;
  programs.jq.enable = true;
  programs.htop = {
    enable = true;
    package =
      (if !pkgs.stdenv.isDarwin then (optimize pkgs.htop) else pkgs.htop);
  };

  home.file = {
    ## create symlink to git repo (instead of nix store) for quicker editing
    "bin/".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.local.nix-repo-path}/scripts";
  };

  home.packages = with pkgs; [
    # pinentry-gnome
    # rofi-pass
    # sshuttle
    # cachix
    asciinema
    asciinema-agg
    awscli2
    aws-vault
    cmatrix # :: Simulates the falling characters theme from The Matrix movie
    direnv
    expect
    gh # GitHub CLI
    gnupg
    greg # :: A command-line podcast aggregator
    inetutils
    jc
    jq
    mediainfo
    mr # :: Multiple Repository management tool
    # ncdu
    # ncspot # :: Cross-platform ncurses Spotify client written in Rust
    neofetch
    nethack
    nixfmt # :: An opinionated formatter for Nix
    nodePackages.prettier
    p7zip
    # passExtensions.pass-checkup
    passh # :: An sshpass alternative for non-interactive ssh auth
    rbenv
    ripgrep # :: replacement for /bin/grep
    spicetify-cli
    spotify-tui
    spotifyd # :: An open source Spotify client running as a UNIX daemon
    termtosvg
    tmux
    tree
    unrar
    unzip
    vcsh # :: Version Control System for $HOME
    w3m
    wget
    youtube-dl # :: download web videos by URL from many websites
    yt-dlp
    zip
    epr
  ];

  xdg.configFile = {
    "htop/htoprc".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.local.nix-repo-path}/dotfiles/htop/htoprc";
    "mpv/motioninterpolation.vpy".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.local.nix-repo-path}/dotfiles/mpv/motioninterpolation.vpy";
  };

  home.file = {
    ".tmux.conf".source = ../dotfiles/tmux.conf;
    ".lein/profiles.clj".source = ../dotfiles/lein/profiles.clj;
  };

  home.sessionPath = [
    "${config.home.homeDirectory}/bin"
    "${config.home.homeDirectory}/bin.local"
    "${config.home.homeDirectory}/.node_modules/bin"
  ];

  programs.git = {
    enable = true;
    userName = "Jeff Workman";
    userEmail = "jeff.workman@gmail.com";
    extraConfig = {
      # core.editor = "emacsclient -t";
      push = { default = "current"; };
      pull = { rebase = true; };
      url."https://aur.archlinux.org/".insteadOf = "aur://";
      url."git@github.com:".insteadOf = "gh://";
      url."git@bitbucket.org:".insteadOf = "bb://";
    };
  };
}
