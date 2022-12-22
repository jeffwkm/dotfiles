{ pkgs, config, lib, inputs, ... }:
with lib;
with config.util;
let
  cfg = config.home.local.emacs;
  emacsPkg = pkgs.emacs;
in {
  # options = {
  #   home.emacs = {
  #     enable = mkBoolOpt true;
  #     install = mkBoolOpt false;
  #   };
  # };

  config = {
    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

    home.sessionVariables = {
      DOOMDIR = "${config.xdg.configHome}/doom-config";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
    };

    home.packages = with pkgs;
      [
        (ripgrep.override { withPCRE2 = true; })
        gnutls
        ispell
        imagemagick
        zstd
        nodePackages.javascript-typescript-langserver
        sqlite
        editorconfig-core-c
      ] ++ optional (cfg.install) emacs;

    nixpkgs.overlays =
      mkIf cfg.install [ (final: prev: { final.emacs = emacsPkg; }) ];

    xdg = {
      enable = true;
      configFile = {
        "doom-config/config.el".source = ./doom.d/config.el;
        "doom-config/init.el".source = ./doom.d/init.el;
        "doom-config/packages.el".source = ./doom.d/packages.el;
        "doom-config/auto-margin.el".source = ./doom.d/auto-margin.el;
        "doom-config/fringe.el".source = ./doom.d/fringe.el;
        "doom-config/ligature.el".source = ./doom.d/ligature.el;
        "doom-config/org-notify.el".source = ./doom.d/org-notify.el;

        # "emacs" = {
        #   source = inputs.doom-emacs;
        #   onChange = "${pkgs.writeShellScript "doom-change" ''
        #     export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
        #     export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
        #     if [ ! -d "$DOOMLOCALDIR" ]; then
        #       ~/.config/emacs/bin/doom -y install
        #         else
        #       ~/.config/emacs/bin/doom -y sync -u
        #     fi
        #   ''}";
        # };
      };
    };
  };
}
