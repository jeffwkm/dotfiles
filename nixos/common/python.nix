{ config, lib, pkgs, ... }:
let
  amdgpu-fan = with import <nixpkgs> { };
    pkgs.python3Packages.callPackage ../amdgpu-fan.nix { };
  python3-packages = (python-packages:
    with python-packages; [
      aioharmony
      amdgpu-fan
      black
      editorconfig
      isort
      jsbeautifier
      lsprotocol
      mpv
      nose
      openai
      openaiauth
      openai-whisper
      pandas
      pip
      pulsectl
      pyflakes
      pytest
      python-lsp-server
      python-mpv-jsonipc
      requests
      setuptools
      todoist
      vapoursynth
    ]);
  python3-custom = pkgs.python3.withPackages python3-packages;
in {
  environment.systemPackages = with pkgs; [
    pipenv
    python2
    (if !config.local.cloud then python3-custom else python3)
  ];
}
