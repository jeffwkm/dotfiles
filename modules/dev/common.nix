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
      })
      (final: prev: {
        nix-direnv = prev.nix-direnv.override { enableFlakes = true; };
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
          openai
          babashka
          cmake
          shellcheck
          shfmt
          direnv
          editorconfig-checker
          editorconfig-core-c
          gnumake
          nodejs
          nil
          rnix-lsp
          nixfmt
          rbenv
          subversion
        ] ++ optionals (!darwin) [ gcc ];
    };
  };
}
