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
      # mpv from pkgs-stable
      (final: prev:
        let
          mpvOpts = {
            scripts = with final.mpvScripts; [
              autoload
              convert
              mpris
              mpv-playlistmanager
              # videoclip
              # cutter
              # thumbnail
              thumbfast
              uosc
            ];
            extraMakeWrapperArgs = optionals cfg.vapoursynth [
              "--prefix"
              "LD_LIBRARY_PATH:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
            ];
          };
          wrapMpv = mpv-unwrapped:
            final.pkgs-stable.wrapMpv mpv-unwrapped mpvOpts;
          withVS = mpv-unwrapped:
            mpv-unwrapped.override { vapoursynthSupport = cfg.vapoursynth; };
        in {
          mpv = pipe final.mpv-unwrapped [ withVS wrapMpv (optimize config) ];
        })
    ];

    # nixpkgs.overlays = [
    #   # customize mpv build for plugins and vapoursynth-mvtools
    #   # mpv from nixpkgs-unstable
    #   (final: prev: {
    #     mpv = pkgs.pkgs-stable.mpv.override {
    #       scripts = with final.mpvScripts; [
    #         autoload
    #         convert
    #         mpris
    #         mpv-playlistmanager
    #         # videoclip
    #         # cutter
    #         # thumbnail
    #         thumbfast
    #         uosc
    #       ];
    #       extraMakeWrapperArgs = optionals cfg.vapoursynth [
    #         "--prefix"
    #         "LD_LIBRARY_PATH:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
    #       ];
    #       mpv = optimize config (prev.mpv.unwrapped.override {
    #         vapoursynthSupport = cfg.vapoursynth;
    #       });
    #     };
    #   })
    # ];

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let
        link = config.lib.file.mkOutOfStoreSymlink;
        mpvExtraDarwin = if darwin then ''
          input-ipc-server=/tmp/mpvsocket
        '' else
          "";
        mpvExtraVapoursynth = lib.optionalString cfg.vapoursynth ''
          vf=format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '';
        inputExtraVapoursynth = lib.optionalString cfg.vapoursynth ''
          Ctrl+i vf toggle format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
        '';
        mpvRateSh = "${user.home}/bin.local/mpv-rate.sh";
        runMpvRate = arg: ''
          Ctrl+${arg} run "${mpvRateSh}" "''${filename}" "''${working-directory}" "''${path}" "${arg}"
        '';
        inputExtraMpvRate =
          foldl' (c: arg: c + runMpvRate (toString arg)) "" (range 1 5);
        mpvExtra = mpvExtraDarwin + mpvExtraVapoursynth;
        inputExtra = inputExtraVapoursynth + inputExtraMpvRate;
      in {
        home.packages = with pkgs;
          optionals (!darwin) [ mpv mpvc celluloid ffmpeg ]
          ++ optionals cfg.vapoursynth [ vapoursynth vapoursynth-mvtools ];

        xdg.configFile = {
          "mpv/mpv.conf".text = (readFile ./mpv.conf) + mpvExtra
            + cfg.extraConf;
          "mpv/input.conf".text = (readFile ./input.conf) + inputExtra;
          "mpv/script-opts/stats.conf".source =
            link "${pwd}/script-opts/stats.conf";
          "mpv/lua-settings/mpv_thumbnail_script.conf".text =
            replaceStrings [ "__CACHE_DIR__" ]
            [ "${user.home}/.cache/mpv-thumbnail" ]
            (readFile ./lua-settings/mpv_thumbnail_script.conf);
          "mpv/motioninterpolation.vpy".source =
            link "${pwd}/motioninterpolation.vpy";
        };
      };
  };
}
