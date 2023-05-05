{ inputs, lib, ... }:
let
  inherit (lib.my.import) importModule importModules;
  inherit (lib.my.modules) mapModules mapModulesRec' ignoreDarwin ignoreLinux;
  inherit (inputs) home-manager nixpkgs;
  inherit (nixpkgs.lib) nixosSystem;
  inherit (inputs.darwin.lib) darwinSystem;
  # inherit (inputs.home-manager.lib) homeManagerConfiguration;
  homeManagerStateVersion = "22.11";
in rec {
  systemHomeManagerModules =
    ({ extraModules ? [ ], darwin, system, nixpkgs, nixpkgsConfig }: {
      home-manager = (if darwin then
        home-manager.darwinModules.home-manager
      else
        home-manager.nixosModules.home-manager);
      hm-config = ({ config, ... }: {
        nixpkgs = nixpkgsConfig;
        nix.nixPath = lib.mkIf (darwin) {
          "darwin-config" = "${config.host.config-dir}";
          # "darwin-config" = "${configDir}";
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
            config = { home.stateVersion = homeManagerStateVersion; };
          };
        };
      });
    });

  mkHost = { system, nixpkgs, nixpkgsConfig, darwin }:
    path:
    let
      mkSystem = if darwin then darwinSystem else nixosSystem;
      ignore = if darwin then ignoreLinux else ignoreDarwin;
      hostname = (lib.removeSuffix ".nix" (baseNameOf path));
    in mkSystem {
      inherit system;
      specialArgs = { inherit lib inputs system; };
      # pkgs = nixpkgs;
      modules = [
        ({ lib, config, ... }: {
          options =
            (import ../hosts/options.nix { inherit lib config; }).options;
          config.host.darwin = darwin;
          config.host.name = hostname;
        })
        { config = { networking.hostName = lib.mkDefault hostname; }; }
      ] ++ [ path ../nix-config.nix ] ++ (lib.attrValues
        (systemHomeManagerModules {
          inherit system nixpkgs nixpkgsConfig darwin;
        })) ++ (mapModulesRec' (toString ../modules) importModule ignore);
    };

  mapHosts = dir:
    attrs@{ system, nixpkgs, nixpkgsConfig, darwin }:
    mapModules dir (hostPath: mkHost attrs hostPath);

  mapHostsNixOS = mapHosts ../hosts/nixos;
  mapHostsDarwin = mapHosts ../hosts/darwin;
}
