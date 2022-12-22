{ config, lib, pkgs, ... }:
let
  python3-packages = (python-packages:
    with python-packages; [
      pandas
      requests
      pulsectl
      pip
      # pyatv
      editorconfig
      isort
      pytest
      jsbeautifier
      mpv
      python-mpv-jsonipc
      aioharmony
      todoist
      # swaytools
      pyflakes
      nose
      setuptools
      black
    ]);
  python3-custom = pkgs.python3.withPackages python3-packages;
in {
  environment.systemPackages = with pkgs; [ pipenv python2 python3-custom ];
}
