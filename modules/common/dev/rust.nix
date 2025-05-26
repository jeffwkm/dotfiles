{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) dev;
  cfg = config.modules.dev.rust;
in {
  options.modules.dev.rust = {
    enable = mkBoolOpt dev.enable-all;
    rustup = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.rust-overlay.overlays.default ];

    home-manager.users.${user.name} = {
      home.packages = with pkgs;
        [ rust-bindgen ] ++ (if cfg.rustup then
          [ rustup ]
        else
          [
            (rust-bin.stable.latest.default.override {
              extensions = [
                "rust-src"
                "rust-analyzer"
                "rustfmt"
                "rust-analysis"
                "clippy"
              ];
            })
          ]);
    };
  };
}
