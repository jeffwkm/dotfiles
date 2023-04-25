{ config, pkgs, ... }: {
  imports = [ ./emacs.nix ./python.nix ./ruby.nix ./system.nix ./postgres.nix ];
}
