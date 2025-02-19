{ lib, ... }:
let
  inherit (lib) foldl' optional optionals;
  addFlagC = pkg: flag:
    pkg.overrideAttrs (attrs: {
      NIX_CFLAGS_COMPILE = (attrs.NIX_CFLAGS_COMPILE or "") + " ${flag}";
      CFLAGS = (attrs.CFLAGS or "") + " ${flag}";
      NIX_ENFORCE_NO_NATIVE = 0;
    });
  addFlagsC = pkg: flags: foldl' (pkg: flag: addFlagC pkg flag) pkg flags;
  addFlagRust = pkg: flag:
    pkg.overrideAttrs (attrs: {
      RUSTFLAGS = (attrs.RUSTFLAGS or "") + " ${flag}";
      NIX_ENFORCE_NO_NATIVE = 0;
    });
  addFlagsRust = pkg: flags: foldl' (pkg: flag: addFlagRust pkg flag) pkg flags;
  addFlags = pkg: { c, rust }: addFlagsRust (addFlagsC pkg c) rust;
in rec {
  optimizePkg = { enable ? true, level ? 3, native ? true }:
    pkg:
    if (!enable) then
      pkg
    else
      addFlags pkg {
        c = (optional (level == 2) "-O2" ++ optional (level == 3) "-O3"
          ++ optionals (level > 3) [ "-Ofast" "-fno-finite-math-only" ]
          ++ optionals native [ "-mtune=native" "-march=native" ]);
        rust = (optionals (level == 2) [ "-C" "opt-level=2" ]
          ++ optionals (level > 2) [ "-C" "opt-level=3" ]
          ++ optionals native [ "-C" "target-cpu=native" ]);
      };
  optimize = config: pkg: optimizePkg { enable = config.host.optimize; } pkg;
  wrapOptimize = config: pkgname:
    (final: prev: { final.pkgname = optimize config prev.pkgname; });
}
