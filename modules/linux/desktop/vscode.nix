{ config, lib, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) dev programs;
  cfg = programs.vscode;
in {
  options.modules.programs.vscode.enable = mkBoolOpt false;

  imports = [ inputs.vscode-server.nixosModule ];

  config = mkIf cfg.enable {
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
