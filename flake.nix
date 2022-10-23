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
        attrValues makeOverridable optionalAttrs singleton mkIf;

      # nixpkgs = inputs.nixpkgs-unstable;

      # mkPkgs = pkgs: extraOverlays:
      #   import pkgs {
      #     config.allowUnfree = true; # forgive me Stallman senpai
      #     overlays = extraOverlays ++ (attrValues self.overlays);
      #   };
      # pkgs = mkPkgs nixpkgs [ self.overlay ];
      # pkgs' = mkPkgs nixpkgs [ ];

      # lib = nixpkgs.lib.extend (self: super: {
      #   my = import ./lib {
      #     inherit pkgs inputs;
      #     lib = self;
      #   };
      # });

      nixpkgs = inputs.nixpkgs-unstable;

      importModule = path:
        { lib, config, pkgs, ... }:
        import path { inherit lib config pkgs inputs; };

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
          (final: prev: {
            final.stdenv = prev.fastStdenv.mkDerivation { name = "env"; };
          })
          (final: prev: {
            # Use packages from nixpkgs-stable
            inherit (final.pkgs-stable) rustracer;
          })
          (final: prev: {
            # Use packages from nixpkgs-unstable
            inherit (final.pkgs-unstable) zsh;
          })
          # Sub in x86 version of packages that don't build on Apple Silicon yet
          (final: prev:
            (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
              # inherit (final.pkgs-x86) idris2;
            }))
          inputs.emacs-overlay.overlay
        ];
      };

      homeManagerStateVersion = "22.11";

      nixConfigRelativePath = ".config/nixpkgs";

      primaryUserInfo = {
        username = "jeff";
        full-name = "Jeff Workman";
        email = "jeff.workman@gmail.com";
      };

      systemHomeManagerModules = ({ extraModules ? [ ], darwin, ... }: {
        home-manager = (if darwin then
          home-manager.darwinModules.home-manager
                        else
                          home-manager.nixosModules.home-manager);
        hm-config = ({ config, ... }:
          let
            username = primaryUserInfo.username;
            homePrefix = (if darwin then "/Users" else "/home");
            homeDir = "${homePrefix}/${username}";
            configDir = "${homeDir}/${nixConfigRelativePath}";
          in {
            nixpkgs = nixpkgsConfig;
            nix.nixPath = mkIf (darwin) {
              "darwin-config" = "${configDir}";
              nixpkgs = "${inputs.nixpkgs-unstable}";
            };
            users.users.${username}.home = homeDir;
            home-manager = {
              # extraSpecialArgs = { mylib = lib.my; };
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = {
                # inherit (config) user;
                imports = (attrValues (if darwin then
                  self.homeManagerModulesMac
                                       else
                                         self.homeManagerModulesLinux)) ++ extraModules;
                home.stateVersion = homeManagerStateVersion;
                home.local.primary-user = primaryUserInfo;
                home.local.nix-repo-path = configDir;
              };
            };
            nix.registry.my.flake = self;
          });
      });
    in {
      nixosConfigurations = rec {
        bootstrap-x86 = makeOverridable nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/bootstrap.nix { nixpkgs = nixpkgsConfig; } ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-linux"; };

        jeff-nixos = nixosSystem {
          system = "x86_64-linux";
          modules = (attrValues (self.nixosModules
                                 // (systemHomeManagerModules {
                                   darwin = false;
                                   extraModules = [ (importModule ./home/gui) ];
                                 }))) ++ [
                                   { local.primary-user = primaryUserInfo; }
                                   hyprland.nixosModules.default
                                   ./nixos/machines/jeff-nixos.nix
                                 ];
        };

        jeff-home = nixosSystem {
          system = "x86_64-linux";
          modules = (attrValues self.nixosModules) ++ (attrValues
            (systemHomeManagerModules {
              darwin = false;
              extraModules = [ (importModule ./home/gui) ];
            })) ++ [
              { local.primary-user = primaryUserInfo; }
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

        jeff-m1x = darwinSystem {
          system = "aarch64-darwin";
          modules = (attrValues self.darwinModules)
                    ++ (attrValues (systemHomeManagerModules { darwin = true; })) ++ [{
                                                                 local.primary-user = primaryUserInfo;
                                                                 networking.computerName = "Jeff-M1X";
                                                                 networking.hostName = "jeff-m1x";
                                                                 networking.knownNetworkServices =
                                                                   [ "Wi-Fi" "USB 10/100/1000 LAN" ];
                                                               }];
        };
      };

      # home-manager config for Linux cloud VMs
      # > nix build .#homeConfigurations.jeff.activationPackage ; ./result/activate
      homeConfigurations.jeff = home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs-unstable {
          system = "x86_64-linux";
          inherit (nixpkgsConfig) config overlays;
        };
        modules = (attrValues self.homeManagerModulesLinux) ++ singleton
          ({ config, ... }: {
            home.username = config.local.primary-user.username;
            home.homeDirectory = "/home/${config.home.username}";
            home.stateVersion = homeManagerStateVersion;
            home.local.primary-user = primaryUserInfo;
            home.local.nix-repo-path =
              "${config.home.homeDirectory}/${nixConfigRelativePath}";
            home.emacs.install = false;
            programs.zsh.prezto.prompt.theme = "steeef";
          });
      };

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

      sharedModules = {
        local-util = importModule ./util;
        local-options = importModule ./options;
      };

      nixosModules = {
        local-bootstrap = importModule ./nixos/bootstrap.nix;
        local-common = importModule ./nixos/common;
      } // self.sharedModules // (systemHomeManagerModules { darwin = false; });

      darwinModules = {
        local-bootstrap = importModule ./darwin/bootstrap.nix;
        local-homebrew = importModule ./darwin/homebrew.nix;
        local-dev = importModule ./darwin/dev.nix;
        local-postgres = importModule ./darwin/postgres.nix;
        local-system = importModule ./darwin/system;
        local-ui = importModule ./darwin/ui;
      } // self.sharedModules // (systemHomeManagerModules { darwin = true; });

      sharedModulesHome = rec {
        local-util = importModule ./util;
        local-common = importModule ./home;
        local-emacs = importModule ./home/emacs;
        home-local-options = { lib, config, ... }: {
          options.home.local = (self.sharedModules.local-options {
            inherit lib config;
            pkgs = nixpkgs;
          }).options.local;
        };
      };

      homeManagerModulesMac = {
        jeff-common-mac = importModule ./home/mac;
      } // self.sharedModulesHome;

      homeManagerModulesLinux = {
        jeff-common-linux = importModule ./home/linux;
        jeff-clojure = importModule ./home/clojure.nix;
      } // self.sharedModulesHome;
    };
}
