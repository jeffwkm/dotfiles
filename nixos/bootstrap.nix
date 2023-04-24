{ config, pkgs, ... }: {
  nix.settings.substituters =
    [ "https://cache.nixos.org/" "https://nix-community.cachix.org" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
  imports = [ ../nix-config.nix ];

  # boot.kernelPackages = pkgs.linuxPackages_latest;

  environment.systemPackages = with pkgs; [ firmwareLinuxNonfree ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: rec {
    oraclejdk8 = pkgs.openjdk8;
    jdk8 = pkgs.openjdk8;
    jdk = pkgs.openjdk8;
  };
  nixpkgs.config.permittedInsecurePackages = [
    "nodejs-10.24.1"
    "nodejs-12.22.12"
    # "qtwebkit-5.212.0-alpha4"
    "python-2.7.18.6"
  ];
}
