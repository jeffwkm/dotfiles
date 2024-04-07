{ lib, ... }:
let
  inherit (lib) foldl';
  addFlagC = pkg: flag:
    pkg.overrideAttrs (attrs: {
      NIX_CFLAGS_COMPILE = (attrs.NIX_CFLAGS_COMPILE or "") + " ${flag}";
      CFLAGS = (attrs.CFLAGS or "") + " ${flag}";
    });
  addFlagsC = pkg: flags: foldl' (pkg: flag: addFlagC pkg flag) pkg flags;
  addFlagRust = pkg: flag:
    pkg.overrideAttrs
    (attrs: { RUSTFLAGS = (attrs.RUSTFLAGS or "") + " ${flag}"; });
  addFlagsRust = pkg: flags: foldl' (pkg: flag: addFlagRust pkg flag) pkg flags;
  addFlags = pkg: flagsC: flagsRust:
    addFlagsRust (addFlagsC pkg flagsC) flagsRust;
in rec {
  optimize = config: pkg:
    if config.host.optimize then
      (addFlags pkg [ "-O3" "-march=native" ] [
        "-C"
        "opt-level=3"
        "-C"
        "target-cpu=native"
      ])
    else
      pkg;
  optimizeNative = config: pkg:
    if config.host.optimize then
      (addFlags pkg [ "-march=native" ] [ "-C" "target-cpu=native" ])
    else
      pkg;
  optimizeFast = config: pkg:
    if config.host.optimize then
      (addFlags pkg [ "-march=native" "-Ofast" "-fno-finite-math-only" ] [
        "-C"
        "opt-level=3"
        "-C"
        "target-cpu=native"
      ])
    else
      pkg;
  wrapOptimize = config: pkgname:
    (final: prev: { final.pkgname = optimize config prev.pkgname; });
}
