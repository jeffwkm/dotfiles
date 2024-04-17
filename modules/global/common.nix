{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let inherit (config) user host modules;
in {
  config = {
    environment.systemPackages = with pkgs; [ cachix home-manager ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.file."bin/".source =
        config.lib.file.mkOutOfStoreSymlink "${host.config-dir}/bin";

      home.file.".tmux.conf".source = ../../dotfiles/tmux.conf;
      xdg.configFile."htop/htoprc".source = config.lib.file.mkOutOfStoreSymlink
        "${host.config-dir}/dotfiles/htop/htoprc";

      home.sessionPath = [
        "${user.home}/bin.local"
        "${user.home}/bin"
        "${user.home}/.cargo/bin"
        "${user.home}/.npm-global/bin"
      ];

      programs.htop.enable = true;
      programs.htop.package = optimize config pkgs.htop;
      programs.gpg.enable = true;

      home.packages = with pkgs;
        [
          (optimize config btop)
          (optimize config fd)
          (optimize config lsd)
          asciinema
          asciinema-agg
          autossh
          comma
          curl
          entr
          expect
          glow
          gpg-tui
          graphicsmagick-imagemagick-compat
          greg
          inetutils
          inxi
          jc
          jq
          mediainfo
          mods
          ncdu
          neofetch
          nix-top
          nushell
          nethack
          pass
          p7zip
          procs
          ripgrep
          screenfetch
          speedtest-rs
          termtosvg
          tgpt
          tmux
          tree
          unrar
          unzip
          w3m
          wget
          xz
          yt-dlp
          zip
        ] ++ (with inputs.rippkgs.packages."${pkgs.system}";
          [ rippkgs rippkgs-index ] ++ [ cmatrix tmatrix unimatrix ]);
    };
  };
}
