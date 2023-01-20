{ config, pkgs, ... }: {
  imports = [
    ./audio.nix
    ./emacs.nix
    ./freetype.nix
    ./python.nix
    ./ruby.nix
    ./system.nix
    ./postgres.nix
  ];
}
