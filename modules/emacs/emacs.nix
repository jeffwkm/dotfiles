{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (host) darwin;
  cfg = config.modules.emacs;
  pwd = "${host.config-dir}/modules/emacs";
in {
  options.modules.emacs = {
    enable = mkBoolOpt false;
    install = mkBoolOpt cfg.enable;
    install-in-home = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = optional cfg.install-in-home
      (final: prev: { final.emacs = optimize config prev.emacsNativeComp; })
      ++ optionals modules.wayland.enable [
        inputs.emacs-overlay.overlay
        (final: prev:
          let
            withPkgs = false;
            base = optimize config prev.emacsPgtk;
            emacsPgtkCustom = (prev.emacsPackagesFor base).emacsWithPackages
              (epkgs: optionals withPkgs [ epkgs.vterm epkgs.all-the-icons ]);
          in { emacs = emacsPgtkCustom; })
      ];

    environment.systemPackages = optional cfg.install pkgs.emacs;

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

      home.sessionVariables = {
        DOOMDIR = "${config.xdg.configHome}/doom-config";
        DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
        EDITOR = "emacsclient -t -a emacs";
        VISUAL = "emacsclient -t -a emacs";
      };

      home.packages = with pkgs;
        [ (ripgrep.override { withPCRE2 = true; }) sqlite editorconfig-core-c ]
        ++ optional cfg.install-in-home emacs;

      xdg.configFile = {
        "doom-config/".source =
          config.lib.file.mkOutOfStoreSymlink "${pwd}/doom.d";

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
