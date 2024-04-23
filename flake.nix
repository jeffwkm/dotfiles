{
  description = "Personal system config for nixos, nix-darwin";

  inputs = {
    ## System
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.follows = "nixos-apple-silicon/nixpkgs";
    nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";
    # nixos-apple-silicon.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-2305.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    ## Nix helpers
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    agenix.inputs.home-manager.follows = "home-manager";
    ## Additional sources
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
    hyprpaper.url = "github:hyprwm/hyprpaper";
    hyprpaper.inputs.nixpkgs.follows = "nixpkgs";
    hyprpaper.inputs.hyprlang.follows = "hyprland/hyprlang";
    rippkgs.url = "github:replit/rippkgs";
    rippkgs.inputs.nixpkgs.follows = "nixpkgs";
    rippkgs.inputs.flake-parts.follows = "flake-parts";
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
    spicetify-nix.url = "github:the-argus/spicetify-nix";
    spicetify-nix.inputs.nixpkgs.follows = "nixpkgs";
    spicetify-nix.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      ## extend lib with custom functions
      lib = nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit inputs;
          ## make nixpkgs.lib available and
          ## allow for references between files in ./lib/*.nix
          ## (as long as they don't create an infinite recursion)
          lib = final;
        };
      });

      nixpkgsConfig = { allowUnfree = true; };

      overlays = import ./nix/pkgsets.nix { inherit inputs nixpkgsConfig; };

      mapHosts' = dir: system:
        lib.my.mapHosts dir {
          inherit system nixpkgsConfig;
          overlays = lib.attrValues overlays;
        };
    in {
      inherit inputs lib;

      options = import ./nix/options-to-json.nix {
        pkgs = import nixpkgs { config = nixpkgsConfig; };
        options = self.nixosConfigurations.jeff-nixos.options;
      };

      nixosConfigurations = (mapHosts' ./hosts/nixos "x86_64-linux")
        // (mapHosts' ./hosts/apple "aarch64-linux");

      darwinConfigurations = mapHosts' ./hosts/darwin "aarch64-darwin";
    };
}
