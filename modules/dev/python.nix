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
    environment.systemPackages = [
      (python3'.overrideAttrs (attrs: {
        # make sure this package overrides others
        # meta.priority = pkgs.python3.meta.priority + 1;
        meta.priority = 100;
      }))
    ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ pipenv poetry virtualenv black ];
    };
  });
}
