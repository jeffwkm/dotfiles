{ config, lib, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev programs;
  inherit (host) darwin;
  cfg = programs.vscode;
  enable = cfg.enable && !darwin;
in {
  options.modules.programs.vscode.enable = mkBoolOpt false;

  ## this creates an infinite recursion
  # imports = optional enable inputs.vscode-server.nixosModule;

  config = mkIf enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      programs.vscode = {
        enable = true;
        package = pkgs.vscode.fhsWithPackages (ps:
          with ps;
          [
            babashka
            direnv
            git
            neovim
            nil
            nixfmt-classic
            nodejs
            openssh
            openssl.dev
            pkg-config
            shellcheck
            shfmt
            zlib
            zsh
          ] ++ optionals dev.rust.enable [ rustup ]
          ++ optionals dev.jdk.enable [ clang-tools jdk maven ]
          ++ optionals dev.clojure.enable [
            boot
            clj-kondo
            clojure
            clojure-lsp
            leiningen
          ]);
      };
    };
  };
}
