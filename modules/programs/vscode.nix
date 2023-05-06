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
            shellcheck
            nil
            nixfmt
            git
            zsh
          ]);
      };
    };
  };
}
