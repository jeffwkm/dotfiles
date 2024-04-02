{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let inherit (config) user host;
in {
  environment.systemPackages = with pkgs; [ home-manager ];

  home-manager.users.${user.name} = { config, pkgs, ... }: {
    home.file = {
      ## create symlink to git repo (instead of nix store) for quicker editing
      "bin/".source =
        config.lib.file.mkOutOfStoreSymlink "${config.host.config-dir}/bin";
    };

    home.sessionPath = [
      "${config.user.home}/bin.local"
      "${config.user.home}/bin"
      "${config.user.home}/.cargo/bin"
      "${config.user.home}/.npm-global/bin"
    ];

    home.packages = with pkgs; [
      asciinema
      asciinema-agg
      cmatrix # :: Simulates the falling characters theme from The Matrix movie
      greg # :: A command-line podcast aggregator
      mediainfo
      nethack
      nodePackages.prettier
      passh # :: An sshpass alternative for non-interactive ssh auth
      pomodoro
      speedtest-rs
      starship
      termtosvg
      yt-dlp
    ];

    programs.gpg.enable = true;
  };
}
