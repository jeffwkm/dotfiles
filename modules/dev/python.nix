{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.python;
in {
  options.modules.dev.python = { enable = mkBoolOpt dev.enable-all; };

  config = mkIf cfg.enable (let
    python3-packages = (python-packages:
      with python-packages;
      [
        aioharmony
        black
        editorconfig
        isort
        jsbeautifier
        lsprotocol
        nose
        openai
        openaiauth
        openai-whisper
        pandas
        pip
        pyflakes
        pytest
        python-lsp-server
        python-mpv-jsonipc
        requests
        setuptools
        todoist
      ] ++ optionals (!host.darwin) [ pulsectl vapoursynth mpv ]);
    python3-custom = pkgs.python3.withPackages python3-packages;
  in {
    environment.systemPackages = with pkgs; [ python2 python3-custom ];

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [ pipenv poetry virtualenv black ];
    };
  });
}
