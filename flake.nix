{
  description = "Personal system config (nixos, nix-darwin)";

  inputs = {
    ### System
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-24.05-darwin";
    nixpkgs-2311.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    nixos-apple-silicon = {
      url = "github:tpwrules/nixos-apple-silicon";
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
    ### Additional sources
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      # url = "github:jeffwkm/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    hyprland = { url = "git+https://github.com/hyprwm/Hyprland?submodules=1"; };
    hyprpaper = {
      url = "github:hyprwm/hyprpaper";
      inputs.nixpkgs.follows = "hyprland/nixpkgs";
      inputs.hyprlang.follows = "hyprland/hyprlang";
    };
    hyprland-stable = { url = "github:hyprwm/Hyprland/v0.39.1"; };
    hyprpaper-stable = {
      url = "github:hyprwm/hyprpaper/v0.6.0";
      inputs.nixpkgs.follows = "hyprland-stable/nixpkgs";
      inputs.hyprlang.follows = "hyprland-stable/hyprlang";
    };
    ags = {
      url = "github:aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ags-v1 = {
      url = "github:aylur/ags/v1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nil-server = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, ... }@inputs:
    let
      # extend lib with custom functions under lib.my
      lib = inputs.nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit inputs;
          # * make nixpkgs.lib available within ./lib
          # * allow for references between files in ./lib
          # * lazy evaluation means no infinite recursion here
          #   unless there's a circular reference within ./lib
          lib = final;
        };
      });

      nixpkgsConfig = { allowUnfree = true; };

      overlays = import ./nix/overlays.nix { inherit inputs nixpkgsConfig; };

      mapHosts' = dir: system:
        lib.my.mapHosts dir {
          inherit system nixpkgsConfig self;
          overlays = lib.attrValues overlays;
        };
    in {
      inherit inputs lib;

      nixosConfigurations = (mapHosts' ./hosts/nixos "x86_64-linux")
        // (mapHosts' ./hosts/asahi "aarch64-linux");

      darwinConfigurations = mapHosts' ./hosts/darwin "aarch64-darwin";

      homeConfigurations.default =
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit lib;
          pkgs = inputs.nixpkgs.legacyPackages.${builtins.currentSystem};
          modules = [
            ({ lib, config, ... }: {
              options =
                (import ./hosts/options.nix { inherit lib config; }).options;
              config = {
                home.username = "${config.user.name}";
                home.homeDirectory = "${config.user.home}";
                home.stateVersion = "24.05";
              };
            })
          ];
        };
    };
}
