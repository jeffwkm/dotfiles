{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (builtins) readFile;
  inherit (config) user host;
  inherit (host) darwin;
  cfg = config.modules.programs.mpv;
  pwd = "${host.config-dir}/modules/programs";
in {
  options.modules.programs.mpv = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      # customize mpv build
      (final: prev:
        let
          mpvOpts = {
            scripts = with final.mpvScripts; [
              autoload
              convert
              mpris
              mpv-playlistmanager
            ];
            extraMakeWrapperArgs = optionals (!darwin) [
              "--prefix"
              "LD_LIBRARY_PATH:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
            ];
          };
          withVS = mpv-unwrapped:
            mpv-unwrapped.override { vapoursynthSupport = true; };
          fromGit = mpv-unwrapped:
            mpv-unwrapped.overrideAttrs (old:
              lib.optionalAttrs false {
                # use mpv from github master
                src = prev.fetchFromGitHub {
                  owner = "mpv-player";
                  repo = "mpv";
                  rev = "e76660cc54751726eee041c9b2ebd1beaff68599";
                  sha256 =
                    "sha256-PC6deDra8Gd91CpF5RJSlVrvkmXgrUmfqR29B7DRRUk=";
                };
              });
          mpv-unwrapped = (withVS (fromGit prev.mpv-unwrapped));
          mpv = (prev.wrapMpv mpv-unwrapped mpvOpts);
        in { mpv = optimize config mpv; })
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs;
        optionals (!darwin) [
          mpv
          mpvc
          vapoursynth
          celluloid
          vapoursynth-mvtools
        ];

      xdg.configFile = let
        mpvExtra = (if darwin then ''
          profile=gpu-hq
          no-native-fs
          input-ipc-server=/tmp/mpvsocket
        '' else ''
          profile=gpu-hq
          gpu-context=wayland
          vf=format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '');
        inputExtra = (if darwin then
          ""
        else ''
          I vf toggle format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '');
      in {
        "mpv/mpv.conf".text = (readFile ./mpv/mpv.conf) + mpvExtra;
        "mpv/input.conf".text = (readFile ./mpv/input.conf) + inputExtra;
        "mpv/script-opts/stats.conf".text =
          readFile ./mpv/script-opts/stats.conf;
        "mpv/motioninterpolation.vpy".source =
          config.lib.file.mkOutOfStoreSymlink
          "${pwd}/mpv/motioninterpolation.vpy";
      };
    };
  };
}
