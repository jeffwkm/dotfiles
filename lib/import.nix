{ inputs, lib, ... }:
let modulesPath = "${inputs.nixpkgs}/nixos/modules";
in rec {
  importModule = path:
    { config, options, pkgs, ... }:
    import path { inherit config options pkgs inputs lib modulesPath; };

  importModules = paths: lib.lists.map importModule paths;
}
