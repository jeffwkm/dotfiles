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

    # Nix helpers
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs-unstable";
    agenix.inputs.darwin.follows = "darwin";

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
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs-unstable";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
    nil-server.url = "github:oxalica/nil";
    nil-server.inputs.nixpkgs.follows = "nixpkgs-unstable";
    nil-server.inputs.flake-utils.follows = "flake-utils";
    nil-server.inputs.rust-overlay.follows = "rust-overlay";
  };

  outputs = { self, darwin, home-manager, ... }@inputs:
    let
      inherit (inputs.nixos-unstable.lib) nixosSystem;
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-unstable.lib)
        attrValues makeOverridable optionalAttrs singleton mkIf;

      nixpkgs-unstable = inputs.nixpkgs-unstable;

      nixpkgsConfig = {
        config = {
          allowUnfree = true;
          allowBroken = true;
          packageOverrides = pkgs: {
            clang = pkgs.clang.overrideAttrs
              (attrs: { meta.priority = pkgs.gcc.meta.priority + 1; });
          };
        };
        overlays = attrValues self.overlays;
      };

      pkgsForSystem = system:
        import nixpkgs-unstable {
          inherit system;
          inherit (nixpkgsConfig) config overlays;
        };

      mylib = nixpkgs-unstable.lib.extend (self: super: {
        my = import ./lib {
          lib = self;
          pkgs = pkgsForSystem super.stdenv.system;
          inherit inputs;
        };
      });

      lib = mylib;

      homeManagerStateVersion = "22.11";

      nixConfigRelativePath = ".config/nixpkgs";

      primaryUserInfo = {
        username = "jeff";
        full-name = "Jeff Workman";
        email = "jeff.workman@gmail.com";
      };

      systemHomeManagerModules = ({ extraModules ? [ ], darwin, system, ... }: {
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
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = {
                # inherit (config) user;
                imports = (attrValues (self.homeManagerModulesShared system))
                  ++ (attrValues (if darwin then
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

      inherit (lib.my) importModule;

      modulesPath = "${nixpkgs-unstable}/nixos/modules";

      importModule' = path:
        { config, pkgs, ... }:
        import path { inherit lib config pkgs inputs modulesPath; };
    in {
      lib = lib;

      overlays = {
        # Overlays to add different versions `nixpkgs` into package set
        pkgs-stable = final: prev: {
          final.pkgs-stable = import inputs.nixpkgs-stable {
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
          zsh = if prev.stdenv.isDarwin then
            prev.zsh
          else
            lib.my.optimizeDefault prev.zsh;
        });

        rust = inputs.rust-overlay.overlays.default;
      };

      nixosConfigurations = rec {
        bootstrap-x86 = makeOverridable nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/bootstrap.nix { nixpkgs = nixpkgsConfig; } ] ++ [{
            local.primary-user = primaryUserInfo;
            local.gui = false;
          }];
        };

        bootstrap-arm = bootstrap-x86.override {
          system = "aarch64-linux";
          modules = [{
            local.primary-user = primaryUserInfo;
            local.gui = false;
          }];
        };

        jeff-nixos = let
          system = "x86_64-linux";
          local = {
            primary-user = primaryUserInfo;
            gui = true;
            cloud = false;
            printing = true;
            emacs.enable = true;
            emacs.install-home = false;
            docker = true;
          };
        in nixosSystem {
          inherit system;
          modules = (attrValues (self.sharedModules // self.nixosModules
            // (systemHomeManagerModules {
              inherit system;
              darwin = false;
              extraModules =
                [ (importModule ./home/gui) { home.local = local; } ];
            }))) ++ [
              { local = local; }
              (importModule' ./nixos/machines/jeff-nixos.nix)
              (importModule ./nixos/vfio)
              (importModule ./nixos/gui)
            ];
        };

        jeff-home = let
          system = "x86_64-linux";
          local = {
            primary-user = primaryUserInfo;
            gui = true;
            cloud = false;
            printing = true;
            emacs.enable = true;
            emacs.install-home = false;
            docker = true;
          };
        in nixosSystem {
          inherit system;
          modules = (attrValues (self.sharedModules // self.nixosModules
            // (systemHomeManagerModules {
              inherit system;
              darwin = false;
              extraModules =
                [ (importModule ./home/gui) { home.local = local; } ];
            }))) ++ [
              { local = local; }
              (importModule' ./nixos/machines/jeff-home.nix)
            ];
        };

        jeff-cloud = let
          system = "x86_64-linux";
          local = {
            gui = false;
            cloud = true;
            primary-user = primaryUserInfo;
          };
        in nixosSystem {
          inherit system;
          modules = (attrValues (self.sharedModules // self.nixosModules
            // (systemHomeManagerModules {
              inherit system;
              darwin = false;
              extraModules = [{ home.local = local; }];
            }))) ++ [
              { local = local; }
              (importModule' ./nixos/machines/jeff-cloud.nix)
            ];
        };
      };

      darwinConfigurations = rec {
        # Mininal configurations to bootstrap systems
        bootstrap-x86 = makeOverridable darwinSystem {
          system = "x86_64-darwin";
          modules = [ ./darwin/bootstrap.nix { nixpkgs = nixpkgsConfig; } ]
            ++ [{
              local.primary-user = primaryUserInfo;
              local.gui = false;
            }];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-darwin"; };

        jeff-m1x = let
          system = "aarch64-darwin";
          local = {
            primary-user = primaryUserInfo;
            gui = false;
            cloud = false;
            printing = false;
            emacs.enable = true;
            emacs.install-home = false;
            docker = false;
          };
        in darwinSystem {
          inherit system;
          modules = (attrValues (self.sharedModules // self.darwinModules
            // (systemHomeManagerModules {
              inherit system;
              darwin = true;
              extraModules = [{ home.local = local; }];
            }))) ++ [
              { local = local; }
              {
                networking.computerName = "Jeff-M1X";
                networking.hostName = "jeff-m1x";
                networking.knownNetworkServices =
                  [ "Wi-Fi" "USB 10/100/1000 LAN" ];
              }
            ];
        };
      };

      # home-manager config for Linux cloud VMs
      # > nix build .#homeConfigurations.jeff.activationPackage ; ./result/activate
      homeConfigurations.jeff = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgsForSystem "x86_64-linux";
        modules = (attrValues self.homeManagerModulesLinux) ++ singleton
          ({ config, ... }: {
            home.username = config.local.primary-user.username;
            home.homeDirectory = "/home/${config.home.username}";
            home.stateVersion = homeManagerStateVersion;
            home.local.primary-user = primaryUserInfo;
            home.local.nix-repo-path =
              "${config.home.homeDirectory}/${nixConfigRelativePath}";
            home.local.emacs.install-home = true;
            home.local.gui = false;
            home.local.cloud = true;
            programs.zsh.prezto.prompt.theme = "steeef";
          });
      };

      sharedModules = { local-options = importModule ./options; };

      nixosModules = {
        local-bootstrap = importModule ./nixos/bootstrap.nix;
        local-common = importModule ./nixos/common;
      };

      darwinModules = {
        local-bootstrap = importModule ./darwin/bootstrap.nix;
        local-homebrew = importModule ./darwin/homebrew.nix;
        local-postgres = importModule ./darwin/postgres.nix;
        local-system = importModule ./darwin/system;
        local-ui = importModule ./darwin/ui;
      };

      homeManagerModulesShared = system: {
        local-common = importModule ./home;
        local-emacs = importModule ./home/emacs;
        home-local-options = { lib, config, ... }: {
          options.home.local = (self.sharedModules.local-options {
            inherit lib config;
            pkgs = pkgsForSystem system;
          }).options.local;
        };
      };

      homeManagerModulesMac = { jeff-common-mac = importModule ./home/mac; };

      homeManagerModulesLinux = {
        local-common-linux = importModule ./home/linux;
      };
    };
}
