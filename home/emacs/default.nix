{ pkgs, config, lib, ... }:
let
  emacsBase = pkgs.emacsPgtkNativeComp;
  emacsCustom = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages
    (epkgs: [ epkgs.vterm ]);
  emacsFinal = config.util.optimizeC emacsCustom [ "-march=native" "-O3" ];
in
{
  options = {
    home.emacs.install = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };
  config = {
    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
    home.sessionVariables = {
      DOOMDIR = "${config.xdg.configHome}/doom-config";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
    };
    home.packages = with pkgs; [
      (ripgrep.override { withPCRE2 = true; })
      gnutls
      ispell
      imagemagick
      zstd
      nodePackages.javascript-typescript-langserver
      sqlite
      editorconfig-core-c
      emacs-all-the-icons-fonts
    ] ++ lib.optional (config.home.emacs.install) [ emacsFinal ];

    systemd.user.services.emacs = {
      Unit = {
        Description = "Emacs daemon";
        After = [ "default.target" ];
      };
      Install = { WantedBy = [ "default.target" ]; };
      Service = {
        Type = "simple";
        ExecStart = "${emacsFinal}/bin/emacs --fg-daemon";
        Restart = "no";
        Environment = [
          "DOOMDIR=%h/.config/doom-config"
          "DOOMLOCALDIR=%h/.config/doom-local"
          "LD_LIBRARY_PATH=${pkgs.freetype_subpixel}/lib"
        ];
      };
    };

    xdg = {
      enable = true;
      configFile = {
        "doom-config/config.el".source = ./doom.d/config.el;
        "doom-config/init.el".source = ./doom.d/init.el;
        "doom-config/packages.el".source = ./doom.d/packages.el;
        "doom-config/auto-margin.el".source = ./doom.d/auto-margin.el;
        "doom-config/ligature.el/ligature.el".source = ./doom.d/ligature.el/ligature.el;
        "doom-config/org-notify/org-notify.el".source = ./doom.d/org-notify/org-notify.el;

        # "emacs" = {
        #   source = builtins.fetchGit "https://github.com/hlissner/doom-emacs";
        #   onChange = "${pkgs.writeShellScript "doom-change" ''
        #     export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
        #     export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
        #     if [ ! -d "$DOOMLOCALDIR" ]; then
        #       doom -y install
        #         else
        #       doom -y sync -u
        #     fi
        #   ''}";
        # };
      };
    };
  };
}
