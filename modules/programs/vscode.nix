{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.programs.vscode;
  enable = cfg.enable && !darwin;
in {
  options.modules.programs.vscode = { enable = mkBoolOpt false; };

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
            nixfmt
            nodejs
            openssh
            openssl.dev
            pkg-config
            shellcheck
            shfmt
            zlib
            zsh
          ] ++ optionals modules.dev.rust.enable [ rustup ]
          ++ optionals modules.dev.jdk.enable [ clang-tools jdk maven ]
          ++ optionals modules.dev.clojure.enable [
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
