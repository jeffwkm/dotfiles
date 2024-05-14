{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (builtins) readFile;
  inherit (lib.trivial) pipe;
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = modules.programs.mpv;
  pwd = "${host.config-dir}/modules/common/programs/mpv";
in {
  options.modules.programs.mpv = {
    enable = mkBoolOpt modules.desktop.enable;
    vapoursynth =
      mkBoolOpt (cfg.enable && !darwin && pkgs.system != "aarch64-linux");
    extraConf = mkOpt types.str "";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      # customize mpv build for plugins and vapoursynth-mvtools
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
          withVS = mpv-unwrapped:
            mpv-unwrapped.override { vapoursynthSupport = cfg.vapoursynth; };
        in {
          mpv = pipe prev.mpv-unwrapped [ withVS wrapMpv (optimize config) ];
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
            gpu-context=waylandvk
          '' + (if cfg.vapoursynth then ''
            vf=format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
          '' else
            ""));
        inputExtra = (if cfg.vapoursynth then ''
          I vf toggle format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '' else
          "");
      in {
        "mpv/mpv.conf".text = (readFile ./mpv.conf) + mpvExtra + cfg.extraConf;
        "mpv/input.conf".text = (readFile ./input.conf) + inputExtra;
        "mpv/script-opts/stats.conf".text = readFile ./script-opts/stats.conf;
        "mpv/motioninterpolation.vpy".source =
          config.lib.file.mkOutOfStoreSymlink "${pwd}/motioninterpolation.vpy";
      };
    };
  };
}
