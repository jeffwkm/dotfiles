{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host;
  inherit (host) darwin;
  cfg = config.modules.dev;
in {
  options.modules.dev = {
    enable = mkBoolOpt true;
    enable-all = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev: {
        clang = prev.clang.overrideAttrs
          (attrs: { meta.priority = prev.gcc.meta.priority + 1; });
        # nix-direnv = prev.nix-direnv.override { enableFlakes = true; };
      })
      inputs.nil-server.overlays.nil
    ];

    home-manager.users.${user.name} = {
      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      home.packages = with pkgs;
        [
          babashka
          bun
          cmake
          direnv
          editorconfig-checker
          editorconfig-core-c
          gnumake
          nil
          nixfmt-classic
          nodePackages.stylelint
          nodejs_20
          openai
          python310Packages.grip
          rbenv
          reg
          regctl
          shellcheck
          shfmt
          skopeo
          subversion
        ] ++ optionals (!darwin) [ gcc ]
        ++ optionals (pkgs.system != "aarch64-linux") [ semgrep ];
    };
  };
}
