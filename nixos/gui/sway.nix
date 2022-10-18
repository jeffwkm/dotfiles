{ config, lib, pkgs, ... }:
let
  optimize = config.util.optimizeDefault;
  iccOverlay = (final: prev: {
    ## build sway/wlroots from icc color-profiles branch
    wlroots = optimize (prev.wlroots.overrideAttrs (old: {
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
    sway = optimize (prev.sway.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "akvadrako";
        repo = "sway";
        rev = "afa611719412b351319bec578a8d3775afbc658f";
        sha256 = "1ibsy4402zhr6vvy114yhzqdx0y72s9qdg06izg5xgv7cznlg1gf";
      };
    }));
    sway-unwrapped = optimize (prev.sway-unwrapped.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "akvadrako";
        repo = "sway";
        rev = "afa611719412b351319bec578a8d3775afbc658f";
        sha256 = "1ibsy4402zhr6vvy114yhzqdx0y72s9qdg06izg5xgv7cznlg1gf";
      };
    }));
  });
in { nixpkgs.overlays = [ iccOverlay ]; }
