{ config, lib, pkgs, ... }:
let
  amdgpu-fan = with import <nixpkgs> { };
    pkgs.python3Packages.callPackage ../amdgpu-fan.nix { };
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
      ### local packages
      amdgpu-fan
    ]);
  python3-custom = pkgs.python3.withPackages python3-packages;
in {
  environment.systemPackages = with pkgs; [ pipenv python2 python3-custom ];
}
