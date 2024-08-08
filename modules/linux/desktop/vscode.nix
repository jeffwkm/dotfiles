{ config, lib, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev programs;
  cfg = programs.vscode;
  pwd = "${host.config-dir}/modules/linux/desktop";
in {
  options.modules.programs.vscode.enable = mkBoolOpt false;

  imports = [ inputs.vscode-server.nixosModule ];

  config = mkIf cfg.enable {
    services.vscode-server.enable = true;

    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = config.lib.file.mkOutOfStoreSymlink;
      in {
        xdg.configFile."Cursor/User/settings.json".source =
          link "${pwd}/Cursor/settings.json";
        xdg.configFile."Cursor/User/keybindings.json".source =
          link "${pwd}/Cursor/keybindings.json";
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
