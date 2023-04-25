{ inputs, lib, ... }: rec {
  importModule = path:
    { config, pkgs, ... }:
    import path { inherit config pkgs inputs lib; };

  importModules = paths: lib.lists.map importModule paths;
}
