{ config, lib, pkgs, inputs, ... }:
with lib;
let
  inherit (lib.my) optimize;
  inherit (config) user host;
  inherit (user) home;
  inherit (host) config-dir;
  pwd = "${config-dir}/modules/common";
in {
  config = {
    users.defaultUserShell = pkgs.zsh;

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

        programs.htop.enable = true;
        programs.htop.package = optimize config pkgs.htop;
        programs.gpg.enable = true;

        home.packages = with pkgs;
          [
            (optimize config fd)
            (optimize config lsd)
            asciinema
            asciinema-agg
            autossh
            babashka
            comma
            curl
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
            mediainfo
            mods
            ncdu
            neofetch
            nethack
            nix-top
            nushell
            p7zip
            pass
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
