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

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs;
    [
      asciinema
      asciinema-agg
      autossh
      aws-vault
      awscli2
      cmatrix # :: Simulates the falling characters theme from The Matrix movie
      direnv
      ec2-api-tools
      expect
      gh # GitHub CLI
      gnupg
      greg # :: A command-line podcast aggregator
      inetutils
      jc
      jq
      mediainfo
      mr # :: Multiple Repository management tool
      ncdu
      neofetch
      nethack
      nix-direnv
      nixfmt
      nixpkgs-fmt
      nodePackages.prettier
      openai-full
      p7zip
      passh # :: An sshpass alternative for non-interactive ssh auth
      pomodoro
      procs
      rbenv
      ripgrep # :: replacement for /bin/grep
      speedtest-rs
      starship
      termtosvg
      tmux
      tree
      unrar
      unzip
      vcsh # :: Version Control System for $HOME
      w3m
      wget
      yt-dlp
      zip
    ] ++ lib.lists.optionals config.home.local.gui [
      ncspot # :: Cross-platform ncurses Spotify client written in Rust
      spicetify-cli
      spotify-tui
      spotifyd # :: An open source Spotify client running as a UNIX daemon
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

  home.sessionVariables = {
    AWS_VAULT_BACKEND = "pass";
    AWS_VAULT_PASS_CMD = "pass";
    AWS_VAULT_PASS_PASSWORD_STORE_DIR =
      "${config.home.homeDirectory}/.password-store";
    AWS_VAULT_PASS_PREFIX = "awsvault";
  };

  home.sessionPath = [
    "${config.home.homeDirectory}/bin.local"
    "${config.home.homeDirectory}/bin"
    "${config.home.homeDirectory}/.cargo/bin"
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
