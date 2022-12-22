{ config, lib, pkgs, ... }: {
  options = {
    util = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Shared functionality for local config";
    };
  };
}
