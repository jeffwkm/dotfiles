{ pkgs, config, lib, ... }:
with lib;
let emacsPkg = pkgs.emacs-nox;
in {
  config = {
    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

    home.sessionVariables = {
      DOOMDIR = "${config.xdg.configHome}/doom-config";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
      EDITOR = "emacsclient -t -a emacs";
      VISUAL = "emacsclient -t -a emacs";
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
      ] ++ optional config.home.local.emacs.install-home emacs;

    nixpkgs.overlays = mkIf config.home.local.emacs.install-home
      [ (final: prev: { final.emacs = emacsPkg; }) ];

    xdg = {
      enable = true;
      configFile = {
        "doom-config/".source = config.lib.file.mkOutOfStoreSymlink
          "${config.home.local.nix-repo-path}/home/emacs/doom.d";

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
