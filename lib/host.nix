{ inputs, lib, ... }:
with lib;
let
  inherit (lib.my.import) importModule importModules;
  inherit (lib.my.modules) mapModules mapModulesRec' ignoreDarwin ignoreLinux;
  inherit (inputs) home-manager nixpkgs;
  inherit (nixpkgs.lib) nixosSystem;
  inherit (inputs.darwin.lib) darwinSystem;

  systemHomeManagerModules = darwin: [
    (if darwin then
      home-manager.darwinModules.home-manager
    else
      home-manager.nixosModules.home-manager)
    ({ config, ... }: {
      nix.nixPath = mkIf (darwin) {
        "darwin-config" = "${config.host.config-dir}";
        nixpkgs = "${inputs.nixpkgs}";
      };
      users.users.${config.user.name}.home = "${config.user.home}";

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.${config.user.name} = {
          options =
            (import ../hosts/options.nix { inherit lib config; }).options;
          imports = importModules [ ../nix-config.nix ];
          config = {
            home.stateVersion = "22.11"; # TODO: set this individually per host
          };
        };
      };
    })
  ];
in rec {
  mkHost = { system, nixpkgsConfig, overlays }:
    path:
    let
      darwin = lib.strings.hasSuffix "darwin" system;
      hostname = (removeSuffix ".nix" (baseNameOf path));
      mkSystem = if darwin then darwinSystem else nixosSystem;
      ignore = if darwin then ignoreLinux else ignoreDarwin;
    in mkSystem {
      inherit system;
      specialArgs = { inherit lib inputs system; };
      modules = [
        ({ lib, config, ... }: {
          options =
            (import ../hosts/options.nix { inherit lib config; }).options;
          config = {
            host.darwin = darwin;
            host.name = hostname;
            networking.hostName = mkDefault hostname;
          };
        })
        {
          nixpkgs = {
            config = nixpkgsConfig;
            overlays = overlays;
          };
        }
      ] ++ [ path ../nix-config.nix ] ++ (systemHomeManagerModules darwin)
        ++ (mapModulesRec' (toString ../modules) importModule ignore);
    };

  mapHosts = dir:
    attrs@{ system, nixpkgsConfig, overlays }:
    mapModules dir (hostPath: mkHost attrs hostPath);
}
