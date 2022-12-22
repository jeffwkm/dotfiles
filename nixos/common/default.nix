{ config, pkgs, ... }: {
  imports = [
    ./audio.nix
    ./emacs.nix
    ./python.nix
    ./ruby.nix
    ./system.nix
    ./postgres.nix
  ];

  nixpkgs.config.permittedInsecurePackages = [ "qtwebkit-5.212.0-alpha4" ];
}
