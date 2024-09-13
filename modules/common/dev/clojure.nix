{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.clojure;
in {
  options.modules.dev.clojure = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable {
    modules.dev.jdk.enable = true;

    home-manager.users.${user.name} = {
      home.file = { ".lein/profiles.clj".source = ./_profiles.clj; };

      home.sessionVariables = { LEIN_JVM_OPTS = "-Xms100m -Xmx300m"; };

      home.packages = with pkgs; [
        boot
        clj-kondo
        cljfmt
        clojure
        clojure-lsp
        leiningen
        zprint
      ];
    };
  };
}
