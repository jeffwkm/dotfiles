{ config, lib, pkgs, inputs, ... }:
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
      home.sessionVariables =
        mkIf modules.wayland.enable { NIXOS_OZONE_WL = "1"; };
      programs.vscode = {
        enable = true;
        package = pkgs.vscode.fhsWithPackages (ps:
          with ps; [
            zlib
            openssl.dev
            pkg-config
            nodejs
            babashka
            shfmt
            direnv
            shellcheck
            shfmt
            nil
            nixfmt
            openssh
            git
            zsh
            neovim
          ] ++ optionals modules.dev.rust.enable [
            (rust-bin.stable.latest.default.override {
              extensions =
                [ "rust-src" "rust-analyzer" "rustfmt" "rls" "rust-analysis" ];
            })
          ] ++ optionals modules.dev.jdk.enable [ jdk maven clang-tools ]
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
