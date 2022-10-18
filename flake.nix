{
  description = "Nix config for my systems (nixos, nix-darwin, home-manager)";

  inputs = {
    # Package sets
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Environment/system management
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.utils.follows = "flake-utils";

    # Nix helpers
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";

    # Additional sources
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs-unstable";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland.inputs.nixpkgs.follows = "nixpkgs-unstable";
    doom-emacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
  };

  outputs = { self, darwin, home-manager, flake-utils, hyprland, ... }@inputs:
    let
      inherit (inputs.nixos-unstable.lib) nixosSystem;
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-unstable.lib)
        attrValues makeOverridable optionalAttrs singleton;

      importModule = path:
        { lib, config, pkgs, ... }:
        import path { inherit lib config pkgs inputs; };

      # Configuration for `nixpkgs`
      nixpkgsConfig = {
        config = {
          allowUnfree = true;
          allowBroken = true;
          packageOverrides = pkgs: rec {
            # Set clang lower priority than gcc
            clang = pkgs.clang.overrideAttrs
              (attrs: { meta.priority = pkgs.gcc.meta.priority + 1; });
          };
        };
        overlays = attrValues self.overlays ++ [
          # Sub in x86 version of packages that don't build on Apple Silicon yet
          (final: prev:
            (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
              # inherit (final.pkgs-x86) idris2;
            }))
          (final: prev: {
            final.stdenv = prev.fastStdenv.mkDerivation { name = "env"; };
          })
          (final: prev: {
            # Use packages from nixpkgs-unstable
            inherit (final.pkgs-unstable) zsh;
          })
          (final: prev: {
            # Use packages from nixpkgs-stable
            inherit (final.pkgs-stable) rustracer;
          })
          inputs.emacs-overlay.overlay
        ];
      };

      homeManagerStateVersion = "22.11";

      nixConfigRelativePath = ".config/nixpkgs";

      primaryUserInfo = {
        username = "jeff";
        fullName = "Jeff Workman";
        email = "jeff.workman@gmail.com";
      };

      nixosCommonModules = { extraHomeModules ? [ ] }:
        attrValues self.nixosModules ++ [
          home-manager.nixosModules.home-manager
          ({ config, ... }:
            let
              inherit (config.users) primaryUser;
              homeDir = "/home/${primaryUser.username}";
              configDir = "${homeDir}/${nixConfigRelativePath}";
            in {
              nixpkgs = nixpkgsConfig;
              # `home-manager` config
              users.users.${primaryUser.username}.home = homeDir;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${primaryUser.username} = {
                imports = (attrValues self.homeManagerModules)
                          ++ (attrValues self.homeManagerModulesLinux)
                          ++ extraHomeModules;
                home.stateVersion = homeManagerStateVersion;
                home.user-info = primaryUserInfo // {
                  nixConfigDirectory = configDir;
                };
              };
              # Add a registry entry for this flake
              nix.registry.my.flake = self;
            })
        ];

      # Modules shared by `nix-darwin` configurations
      # (includes config for `home-manager`)
      nixDarwinCommonModules = attrValues self.darwinModules ++ [
        # `home-manager` module
        home-manager.darwinModules.home-manager
        ({ config, ... }:
          let
            inherit (config.users) primaryUser;
            homeDir = "/Users/${primaryUser.username}";
            configDir = "${homeDir}/${nixConfigRelativePath}";
          in {
            nixpkgs = nixpkgsConfig;
            nix.nixPath = {
              "darwin-config" = "${configDir}";
              nixpkgs = "${inputs.nixpkgs-unstable}";
            };
            # `home-manager` config
            users.users.${primaryUser.username}.home = homeDir;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${primaryUser.username} = {
              imports = (attrValues self.homeManagerModules)
                        ++ (attrValues self.homeManagerModulesMac);
              home.stateVersion = homeManagerStateVersion;
              home.user-info = primaryUserInfo // {
                nixConfigDirectory = configDir;
              };
            };
            # Add a registry entry for this flake
            nix.registry.my.flake = self;
          })
      ];
    in {
      nixosConfigurations = rec {
        bootstrap-x86 = makeOverridable nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/bootstrap.nix { nixpkgs = nixpkgsConfig; } ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-linux"; };

        # Personal desktop config
        jeff-nixos = nixosSystem {
          system = "x86_64-linux";
          modules = (nixosCommonModules {
            extraHomeModules = [ (importModule ./home/gui) ];
          }) ++ [
            { users.primaryUser = primaryUserInfo; }
            hyprland.nixosModules.default
            ./nixos/machines/jeff-nixos.nix
          ];
        };

        # Personal fileserver config
        jeff-home = nixosSystem {
          system = "x86_64-linux";
          modules = (nixosCommonModules {
            extraHomeModules = [ (importModule ./home/gui) ];
          }) ++ [
            { users.primaryUser = primaryUserInfo; }
            ./nixos/machines/jeff-home.nix
          ];
        };
      };

      darwinConfigurations = rec {
        # Mininal configurations to bootstrap systems
        bootstrap-x86 = makeOverridable darwinSystem {
          system = "x86_64-darwin";
          modules = [ ./darwin/bootstrap.nix { nixpkgs = nixpkgsConfig; } ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-darwin"; };

        # Personal laptop config (arm64, macOS)
        jeff-m1x = darwinSystem {
          system = "aarch64-darwin";
          modules = nixDarwinCommonModules ++ [{
            users.primaryUser = primaryUserInfo;
            networking.computerName = "Jeff-M1X";
            networking.hostName = "jeff-m1x";
            networking.knownNetworkServices = [ "Wi-Fi" "USB 10/100/1000 LAN" ];
          }];
        };
      };

      # home-manager config for Linux cloud VMs
      # -- "nix build .#homeConfigurations.jeff.activationPackage ; ./result/activate"
      homeConfigurations.jeff = home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs-unstable {
          system = "x86_64-linux";
          inherit (nixpkgsConfig) config overlays;
        };
        modules = (attrValues self.homeManagerModules)
                  ++ (attrValues self.homeManagerModulesLinux) ++ singleton
                    ({ config, ... }: {
                      home.username = config.home.user-info.username;
                      home.homeDirectory = "/home/${config.home.username}";
                      home.stateVersion = homeManagerStateVersion;
                      home.user-info = primaryUserInfo // {
                        nixConfigDirectory =
                          "${config.home.homeDirectory}/${nixConfigRelativePath}";
                      };
                      home.emacs.install = false;
                      programs.zsh.prezto.prompt.theme = "steeef";
                    });
      };

      # `overlays` output (`self.overlays`)
      overlays = {
        # Overlays to add different versions `nixpkgs` into package set
        pkgs-stable = _: prev: {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsConfig) config;
          };
        };
        pkgs-unstable = _: prev: {
          pkgs-unstable = import inputs.nixpkgs-unstable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsConfig) config;
          };
        };

        # Overlay useful on Macs with Apple Silicon
        apple-silicon = _: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            # Add access to x86 packages system is running Apple Silicon
            pkgs-x86 = import inputs.nixpkgs-unstable {
              system = "x86_64-darwin";
              inherit (nixpkgsConfig) config;
            };
          };

        # Overlay to include node packages listed in `./pkgs/node-packages/package.json`
        # Run `nix run my#nodePackages.node2nix -- -14` to update packages.
        nodePackages = _: prev: {
          nodePackages = prev.nodePackages
                         // import ./pkgs/node-packages { pkgs = prev; };
        };
      };

      nixosModules = {
        # Config files
        jeff-util = importModule ./util;
        jeff-bootstrap = importModule ./nixos/bootstrap.nix;
        jeff-common = importModule ./nixos/common;

        # Local modules
        users-primaryUser = import ./modules/users.nix;
      };

      darwinModules = {
        # Config files
        jeff-util = importModule ./util;
        jeff-bootstrap = importModule ./darwin/bootstrap.nix;
        jeff-homebrew = importModule ./darwin/homebrew.nix;
        jeff-dev = importModule ./darwin/dev.nix;
        jeff-postgres = importModule ./darwin/postgres.nix;
        jeff-system = importModule ./darwin/system;
        jeff-ui = importModule ./darwin/ui;

        # Local modules
        users-primaryUser = import ./modules/users.nix;
      };

      homeManagerModules = rec {
        # Config files
        jeff-util = importModule ./util;
        jeff-common = importModule ./home;
        jeff-emacs = importModule ./home/emacs;

        users-primaryUser = import ./modules/users.nix;

        home-user-info = { lib, ... }: {
          options.home.user-info =
            (users-primaryUser { inherit lib; }).options.users.primaryUser;
        };
      };

      homeManagerModulesMac = { jeff-common-mac = importModule ./home/mac; };

      homeManagerModulesLinux = {
        jeff-common-linux = importModule ./home/linux;
        jeff-clojure = importModule ./home/clojure.nix;
      };

      # Add re-export `nixpkgs` packages with overlays.
      # This is handy in combination with `nix registry add my /Users/malo/.config/nixpkgs`
    } // flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages = import inputs.nixpkgs-unstable {
        inherit system;
        inherit (nixpkgsConfig) config;
        overlays = with self.overlays; [
          pkgs-stable
          apple-silicon
          nodePackages
        ];
      };
    });
}
