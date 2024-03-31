{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (builtins) readFile;
  inherit (lib.trivial) pipe;
  inherit (config) user host;
  inherit (host) darwin;
  cfg = config.modules.programs.mpv;
  pwd = "${host.config-dir}/modules/programs";
in {
  options.modules.programs.mpv = {
    enable = mkBoolOpt false;
    vapoursynth =
      mkBoolOpt (cfg.enable && !darwin && pkgs.system != "aarch64-linux");
    git = mkBoolOpt false;
  };

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
            extraMakeWrapperArgs = optionals cfg.vapoursynth [
              "--prefix"
              "LD_LIBRARY_PATH:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
            ];
          };
          wrapMpv = mpv-unwrapped: prev.wrapMpv mpv-unwrapped mpvOpts;
          fromGit = mpv-unwrapped:
            if (!cfg.git) then
              mpv-unwrapped
            else
              mpv-unwrapped.overrideAttrs (old:
                # use mpv from github master
                lib.optionalAttrs false {
                  src = prev.fetchFromGitHub {
                    owner = "mpv-player";
                    repo = "mpv";
                    rev = "e76660cc54751726eee041c9b2ebd1beaff68599";
                    sha256 =
                      "sha256-PC6deDra8Gd91CpF5RJSlVrvkmXgrUmfqR29B7DRRUk=";
                  };
                });
          withVS = mpv-unwrapped:
            mpv-unwrapped.override { vapoursynthSupport = cfg.vapoursynth; };
        in {
          mpv = pipe prev.mpv-unwrapped [
            fromGit
            withVS
            wrapMpv
            (optimize config)
          ];
        })
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs;
        optionals (!darwin) [ mpv mpvc celluloid ]
        ++ optionals cfg.vapoursynth [ vapoursynth vapoursynth-mvtools ];

      xdg.configFile = let
        mpvExtra = (if darwin then ''
          input-ipc-server=/tmp/mpvsocket
        '' else
          ''
            gpu-context=wayland
          '' + (if cfg.vapoursynth then ''
            vf=format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
          '' else
            ""));
        inputExtra = (if cfg.vapoursynth then ''
          I vf toggle format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '' else
          "");
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
