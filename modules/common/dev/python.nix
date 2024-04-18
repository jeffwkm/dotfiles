{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.python;
in {
  options.modules.dev.python.enable = mkBoolOpt dev.enable-all;

  config = mkIf cfg.enable (let
    python3-packages = (python-packages:
      with python-packages;
      [
        black
        catppuccin
        editorconfig
        isort
        jsbeautifier
        lsprotocol
        openai
        pandas
        pip
        python-lsp-server
        python-mpv-jsonipc
        requests
        setuptools
      ] ++ optionals (!host.darwin) [ pulsectl mpv ]
      ++ optionals (!host.darwin && pkgs.system != "aarch64-linux")
      [ vapoursynth ]);
    python3' = pkgs.python3.withPackages python3-packages;
  in {
    environment.systemPackages =
      if cfg.enable then [ python3' ] else pkgs.python3;

    home-manager.users.${user.name} = mkIf cfg.enable ({ config, pkgs, ... }: {
      home.packages = with pkgs; [ pipenv poetry virtualenv black ];
    });
  });
}
