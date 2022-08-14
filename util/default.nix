{config, lib, pkgs, ...}:
{
  imports = [
    ./compile.nix
    ./lib.nix
  ];
  options = {
    util = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Shared functionality for local config";
    };
  };
}
