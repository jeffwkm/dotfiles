{ inputs, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (inputs.darwin.lib) darwinSystem;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in {
  mkHost = path:
    attrs@{ system ? sys, ... }:
    nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs system; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        ../. # /default.nix
        (import path)
      ];
    };

  mapHosts = dir:
    attrs@{ system ? system, ... }:
    mapModules dir (hostPath: mkHost hostPath attrs);

  mkHostNixOS = path:
    attrs@{ system ? "x86_64-linux", extraModules ? [ ], ... }:
    rootPath:
    nixosSystem {
      inherit system;
      specialArgs = {
        mylib = lib.my;
        inherit lib inputs system rootPath;
      };
      modules = [
        {
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n [ "system" "extraModules" ]) attrs)
        ../nixos/bootstrap.nix
        (import path)
      ] ++ extraModules;
    };

  mapHostsNixOS = dir:
    attrs@{ system ? system, ... }:
    rootPath:
    mapModules dir (hostPath: mkHostNixOS hostPath attrs rootPath);

  mkHostDarwin = path:
    attrs@{ system ? "aarch64-darwin", extraModules, ... }:
    rootPath:
    darwinSystem {
      inherit system;
      specialArgs = { inherit lib inputs system rootPath; };
      modules = [
        {
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n [ "system" "extraModules" ]) attrs)
        ../darwin/bootstrap.nix
        (import path)
      ] ++ extraModules;
    };

  mapHostsDarwin = dir:
    attrs@{ system ? system, ... }:
    rootPath:
    mapModules dir (hostPath: mkHostDarwin hostPath attrs rootPath);

  mkHostHM = path:
    attrs@{ system ? "x86_64-linux", extraModules ? [ ], pkgs, ... }:
    rootPath:
    homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {
        mylib = lib.my;
        inherit rootPath;
      };
      modules = [
        (filterAttrs (n: v: !elem n [ "system" "extraModules" "pkgs" ]) attrs)
        ../home/bootstrap.nix
        (import path)
      ] ++ extraModules;
    };

  mapHostsHM = dir:
    attrs@{ system ? system, ... }:
    rootPath:
    mapModules dir (hostPath: mkHostHM hostPath attrs rootPath);

}
