{ lib, inputs, nixpkgsConfig, ... }:
let
  inherit (lib) optionalAttrs;
  inherit (lib.my) optimizeDefault;
  inherit (inputs) rust-overlay nixpkgs-stable nixpkgs-unstable;
in {
  # Overlays to add different versions `nixpkgs` into package set
  pkgs-stable = final: prev: {
    final.pkgs-stable = import nixpkgs-stable {
      inherit (prev.stdenv) system;
      inherit (nixpkgsConfig) config;
    };
    # inherit (final.pkgs-stable) spotify;
  };

  # Overlay useful on Macs with Apple Silicon
  apple-silicon = final: prev:
    optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
      # Add access to x86 packages system is running Apple Silicon
      final.pkgs-x86 = import nixpkgs-unstable {
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

  direnv = (final: prev: {
    nix-direnv = prev.nix-direnv.override { enableFlakes = true; };
  });

  fastStdenv = (final: prev: {
    final.stdenv = prev.fastStdenv.mkDerivation { name = "env"; };
  });

  optimizeZsh = (final: prev: {
    zsh = if prev.stdenv.isDarwin then prev.zsh else optimizeDefault prev.zsh;
  });

  rust = rust-overlay.overlays.default;
}
