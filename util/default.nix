{ config, lib, pkgs, ... }: {
  imports = [ ./options.nix ./compile.nix ./lib.nix ];
}
