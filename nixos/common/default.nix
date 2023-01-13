{ config, pkgs, ... }: {
  imports = [
    ./audio.nix
    ./emacs.nix
    ./python.nix
    ./ruby.nix
    ./system.nix
    ./postgres.nix
  ];
}
