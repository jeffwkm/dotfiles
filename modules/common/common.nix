{ config, lib, pkgs, ... }:
with lib;
let
  inherit (lib.my) mkBoolOpt optimize;
  inherit (config) user host modules;
  inherit (user) home;
  inherit (host) config-dir darwin;
  pwd = "${config-dir}/modules/common";
in {
  options = { modules.guix.enable = mkBoolOpt false; };

  config = {
    nixpkgs.config.permittedInsecurePackages =
      [ "nodejs-10.24.1" "nodejs-12.22.12" "python-2.7.18.6" ];

    environment.systemPackages = with pkgs; [ cachix home-manager ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = config.lib.file.mkOutOfStoreSymlink;
      in {
        home.sessionPath = [
          "${home}/bin.local"
          "${home}/bin"
          "${home}/.cargo/bin"
          "${home}/.npm-global/bin"
        ];

        home.file."bin/".source = link "${config-dir}/bin";
        home.file.".tmux.conf".source = ./tmux.conf;
        xdg.configFile."htop/htoprc".source = link "${pwd}/htoprc";
        xdg.configFile."bat/config".source = link "${pwd}/bat.config";

        programs.htop.enable = true;
        programs.htop.package = optimize config pkgs.htop;
        programs.gpg.enable = true;

        programs.imv.enable = !darwin;
        programs.pqiv.enable = !darwin;

        home.packages = with pkgs;
          [
            (optimize config fd)
            (optimize config lsd)
            aichat
            asciinema
            asciinema-agg
            autossh
            babashka
            bat
            comma
            curl
            datasette
            entr
            expect
            glow
            gnugrep
            gnused
            gpg-tui
            graphicsmagick-imagemagick-compat
            greg
            inetutils
            inxi
            jc
            jq
            llm
            mediainfo
            mods
            ncdu
            neofetch
            nethack
            nushell
            p7zip
            pass
            procs
            python3Packages.markdownify
            ripgrep
            rippkgs
            screenfetch
            speedtest-rs
            termtosvg
            tgpt
            timg
            tmux
            tree
            unrar
            unzip
            w3m
            wget
            xz
            yt-dlp
            zip
          ] ++ [ cmatrix tmatrix unimatrix ]
          ++ optionals (modules.guix.enable) [ guix ];
      };
  };
}
