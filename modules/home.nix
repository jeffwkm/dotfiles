{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let inherit (config) user host;
in {
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

    # programs.ssh = {
    #   enable = true;
    #   matchBlocks = {
    #     "jeff-aws" = {
    #       match = "host jeff-aws,jeff-aws.ddns.net";
    #       hostname = "jeff-aws.ddns.net";
    #       user = "jeff";
    #     };
    #     "sysrev" = {
    #       match = "host sysrev,sysrev.com";
    #       hostname = "sysrev.com";
    #       user = "ubuntu";
    #     };
    #     "staging" = {
    #       match = "host staging,staging.sysrev.com";
    #       hostname = "staging.sysrev.com";
    #       user = "ubuntu";
    #     };
    #   };
    #   forwardAgent = true;
    #   compression = true;
    #   serverAliveInterval = 30;
    #   serverAliveCountMax = 3;
    #   extraConfig = ''
    #     ExitOnForwardFailure yes
    #   '';
    # };

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
