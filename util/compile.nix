{ pkgs, ... }:
let
  addFlagC = pkg: flag:
    pkg.overrideAttrs (attrs: {
      NIX_CFLAGS_COMPILE = (attrs.NIX_CFLAGS_COMPILE or "") + " ${flag}";
      CFLAGS = (attrs.CFLAGS or "") + " ${flag}";
    });
  addFlagsC = pkg: flags:
    pkgs.lib.foldl' (pkg: flag: addFlagC pkg flag) pkg flags;
  addFlagRust = pkg: flag:
    pkg.overrideAttrs
    (attrs: { RUSTFLAGS = (attrs.RUSTFLAGS or "") + " ${flag}"; });
  addFlagsRust = pkg: flags:
    pkgs.lib.foldl' (pkg: flag: addFlagRust pkg flag) pkg flags;
  addFlags = pkg: flagsC: flagsRust:
    addFlagsRust (addFlagsC pkg flagsC) flagsRust;
  rustFlags = [ "-C" "opt-level=3" "-C" "target-cpu=native" ];
  cFlags = [ "-O3" "-march=native" ];
in {
  util.optimizeC = addFlagsC;
  util.optimizeDefault = pkg: addFlags pkg cFlags rustFlags;
  util.optimizeFast = pkg:
    addFlags pkg [ "-march=native" "-Ofast" "-fno-finite-math-only" ] rustFlags;
}
