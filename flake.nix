{
  description = "Personal system config (nixos, nix-darwin)";

  inputs = {
    ### System
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-24.05-darwin";
    nixos-apple-silicon = {
      url = "github:tpwrules/nixos-apple-silicon";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    ### Nix helpers
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "darwin";
      inputs.home-manager.follows = "home-manager";
    };
    ### Additional sources
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
      inputs.flake-utils.follows = "flake-utils";
    };
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprpaper = {
      url = "github:hyprwm/hyprpaper";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hyprlang.follows = "hyprland/hyprlang";
    };
    ags = {
      url = "github:Aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nil-server = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    vscode-server = {
      url = "github:nix-community/nixos-vscode-server";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, ... }@inputs:
    let
      ## extend lib with custom functions
      lib = inputs.nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit inputs;
          ## make nixpkgs.lib available and
          ## allow for references between files in ./lib/*.nix
          ## (as long as they don't create an infinite recursion)
          lib = final;
        };
      });

      nixpkgsConfig = { allowUnfree = true; };

      overlays = import ./nix/overlays.nix { inherit inputs nixpkgsConfig; };

      mapHosts' = dir: system:
        lib.my.mapHosts dir {
          inherit system nixpkgsConfig;
          overlays = lib.attrValues overlays;
        };
    in {
      inherit inputs lib;

      options = import ./nix/options-to-json.nix {
        pkgs = import inputs.nixpkgs { config = nixpkgsConfig; };
        options = self.nixosConfigurations.jeff-nixos.options;
      };

      nixosConfigurations = (mapHosts' ./hosts/nixos "x86_64-linux")
        // (mapHosts' ./hosts/apple "aarch64-linux");

      darwinConfigurations = mapHosts' ./hosts/darwin "aarch64-darwin";
    };
}
