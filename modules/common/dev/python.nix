{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  inherit (modules) dev;
  cfg = config.modules.dev.python;
  python3-packages = (python-packages:
    with python-packages;
    [
      black
      editorconfig
      isort
      jsbeautifier
      lsprotocol
      pip
      python-lsp-server
      requests
      setuptools
      ipykernel
    ] ++ optionals (!host.darwin && host.gui) [ pulsectl ]
    ++ optionals (!host.minimal) [ catppuccin librosa soundfile openai pandas ]
    ++ optionals
    (!host.darwin && !host.minimal && pkgs.system != "aarch64-linux")
    [ vapoursynth ]);
  python3' = pkgs.pkgs-stable.python3.withPackages python3-packages;
in {
  options.modules.dev.python.enable = mkBoolOpt dev.enable-all;

  config = {
    environment.systemPackages =
      if cfg.enable then [ python3' ] else [ pkgs.python3 ];

    home-manager.users.${user.name} = mkIf cfg.enable ({ config, pkgs, ... }: {
      home.packages = with pkgs; [ pipenv poetry virtualenv black ];
    });
  };
}
