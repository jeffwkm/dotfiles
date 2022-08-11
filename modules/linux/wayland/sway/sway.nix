{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  iccOverlay = (final: prev: {
    ## build sway/wlroots from icc color-profiles branch
    wlroots = optimize config (prev.wlroots.overrideAttrs (old: {
      # mesonFlags = [ "-Dlogind-provider=systemd" "-Dlibseat=disabled" ];
      mesonFlags = [ "-Dauto_features=auto" ];
      src = prev.fetchFromGitHub {
        owner = "akvadrako";
        repo = "wlroots";
        rev = "a0e0dfacd43b7d295b41f0208a7c04f4473f0a8c";
        sha256 = "1m03ns2nfw7gcagch5mlb83xkv6cx73nr4xnz9lxd0narq1knahr";
      };
      buildInputs = old.buildInputs ++ [ prev.lcms2 prev.libuuid ];
    }));
    sway = optimize config (prev.sway.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "akvadrako";
        repo = "sway";
        rev = "afa611719412b351319bec578a8d3775afbc658f";
        sha256 = "1ibsy4402zhr6vvy114yhzqdx0y72s9qdg06izg5xgv7cznlg1gf";
      };
    }));
    sway-unwrapped = optimize config (prev.sway-unwrapped.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "akvadrako";
        repo = "sway";
        rev = "afa611719412b351319bec578a8d3775afbc658f";
        sha256 = "1ibsy4402zhr6vvy114yhzqdx0y72s9qdg06izg5xgv7cznlg1gf";
      };
    }));
  });
  cfg = config.modules.wayland.sway;
in {
  options.modules.wayland.sway = {
    enable = mkBoolOpt modules.wayland.enable;
    icc = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = optional cfg.icc iccOverlay;

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [
        swaybg
        swayidle
        swaylock
        swaytools
        swayr # :: A window switcher (and more) for sway
        swaywsr # :: Automatically change sway workspace names based on their contents
      ];

      xdg.configFile."sway/config" = {
        source = config.lib.file.mkOutOfStoreSymlink
          "${config.host.config-dir}/dotfiles/sway/config";
        onChange = "${pkgs.writeShellScript "sway-change" ''
          set +e
          socket=$(ls /run/user/1000/sway*)
          swaymsg -s "$socket" -- reload || true
        ''}";
      };

      systemd.user.targets.sway = {
        Unit = {
          Description = "sway compositor session";
          BindsTo = [ "graphical-session.target" ];
          Wants = [ "graphical-session-pre.target" ];
          After = [ "graphical-session-pre.target" ];
        };
      };

      systemd.user.services.swayidle = {
        Unit = {
          Description = "Idle manager for Sway compositor";
          PartOf = [ "graphical-session.target" ];
          After = [ "sway.target" ];
        };
        Install = { WantedBy = [ "sway.target" ]; };
        Service = {
          Type = "simple";
          ExecCondition =
            ''${pkgs.bash}/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
          # 'swaylock-fancy --daemonize'
          ExecStart = ''
            ${pkgs.swayidle}/bin/swayidle -w \
                  timeout 1800   'swaylock -f -c 000000' \
                  timeout 3600   'swaymsg "output * dpms off"' \
                  resume         'swaymsg "output * dpms on"' \
                  before-sleep   'swaylock -f -c 000000' \
          '';
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
  };
}
