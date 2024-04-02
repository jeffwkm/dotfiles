{
  description = "Personal system config for nixos, nix-darwin";

  inputs = {
    ## Package sets
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-2305.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";
    ## Environment/system management
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    ## Nix helpers
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    agenix.inputs.home-manager.follows = "home-manager";
    ## Additional sources
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
    rippkgs.url = "github:jeffwk/rippkgs";
    rippkgs.inputs.nixpkgs.follows = "nixpkgs";
    ags.url = "github:Aylur/ags";
    ags.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
    nil-server.url = "github:oxalica/nil";
    nil-server.inputs.nixpkgs.follows = "nixpkgs";
    nil-server.inputs.flake-utils.follows = "flake-utils";
    nil-server.inputs.rust-overlay.follows = "rust-overlay";
    vscode-server.url = "github:nix-community/nixos-vscode-server";
    vscode-server.inputs.nixpkgs.follows = "nixpkgs";
    vscode-server.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      ## extend lib with custom functions
      lib = nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit inputs;
          lib = final;
        };
      });

      nixpkgsConfig = {
        config = {
          allowUnfree = true;
          # allowBroken = true;
        };
        overlays = lib.attrValues self.overlays;
      };

      pkgsForSystem = system:
        import nixpkgs {
          inherit system;
          inherit (nixpkgsConfig) config overlays;
        };

    in {
      inherit inputs lib;

      overlays = {
        fastStdenv = final: prev: {
          final.stdenv = prev.fastStdenv.mkDerivation { name = "env"; };
        };
        pkgs-stable = final: prev: {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsConfig) config;
          };
          # inherit (final.pkgs-stable) steam;
        };
        pkgs-2305 = final: prev: {
          pkgs-2305 = import inputs.nixpkgs-2305 {
            inherit (prev.stdenv) system;
            inherit (nixpkgsConfig) config;
          };
          # inherit (final.pkgs-2305) steam;
        };
        apple-silicon = final: prev:
          lib.optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            # Add access to x86 packages system is running Apple Silicon
            final.pkgs-x86 = import nixpkgs {
              system = "x86_64-darwin";
              inherit (nixpkgsConfig) config;
            };
            # inherit (final.pkgs-x86) firefox;
          };
        # Overlay to include node packages listed in `./pkgs/node-packages/package.json`
        # Run `nix run my#nodePackages.node2nix -- -14` to update packages.
        nodePackages = _: prev: {
          nodePackages = prev.nodePackages
            // import ./pkgs/node-packages { pkgs = prev; };
        };
      };

      nixosConfigurations = (lib.my.mapHosts ./hosts/nixos rec {
        inherit nixpkgsConfig;
        system = "x86_64-linux";
        nixpkgs = pkgsForSystem system;
        darwin = false;
      }) // (lib.my.mapHosts ./hosts/apple rec {
        inherit nixpkgsConfig;
        system = "aarch64-linux";
        nixpkgs = pkgsForSystem system;
        darwin = false;
      });

      darwinConfigurations = lib.my.mapHosts ./hosts/darwin rec {
        inherit nixpkgsConfig;
        system = "aarch64-darwin";
        nixpkgs = pkgsForSystem system;
        darwin = true;
      };
    };
}
