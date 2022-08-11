{config, lib, pkgs, ...}:
let
  addFlagC = pkg: flag:
    pkg.overrideAttrs (attrs: {
      # NIX_CFLAGS_COMPILE = (attrs.NIX_CFLAGS_COMPILE or "") + " ${flag}";
      CFLAGS = (attrs.CFLAGS or "") + " ${flag}";
    });
  addFlagsC = pkg: flags:
    pkgs.lib.foldl' (pkg: flag: addFlagC pkg flag) pkg flags;
  addFlagRust = pkg: flag:
    pkg.overrideAttrs (attrs: {
      RUSTFLAGS = (attrs.RUSTFLAGS or "") + " ${flag}";
    });
  addFlagsRust = pkg: flags:
    pkgs.lib.foldl' (pkg: flag: addFlagRust pkg flag) pkg flags;
  addFlags = pkg: flagsC: flagsRust:
    addFlagsRust (addFlagsC pkg flagsC) flagsRust;
  util = {
    optimizeC = addFlagsC;
    optimizeDefault = pkg: addFlags pkg
      ["-O3" "-march=native"]
      ["-C" "opt-level=3" "-C" "target-cpu=native"];
  };
in {
  options = {
    util = lib.mkOption {
      type = lib.types.attrs;
      default = util;
      description = "Shared functionality for local config";
    };
  };
}
