{ config, pkgs, fetchFromGitHub, ... }: {
  # boot.kernelPackages = pkgs.linuxPackages_latest;
  imports = [ ../modules/nix-config.nix ];
  environment.systemPackages = with pkgs; [ firmwareLinuxNonfree ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: rec {
    oraclejdk8 = pkgs.openjdk8;
    jdk8 = pkgs.openjdk8;
    jdk = pkgs.openjdk8;
    zsh = config.util.optimizeDefault config.util.unstable.zsh;
    clang = pkgs.clang.overrideAttrs (attrs: {
      # Lower priority than gcc.
      meta.priority = pkgs.gcc.meta.priority + 1;
    });
  };
  nixpkgs.overlays = [
    (final: prev: {
      final.stdenv = prev.fastStdenv.mkDerivation { name = "env"; };
    })
  ];
  nixpkgs.config.permittedInsecurePackages = [
    "nodejs-10.24.1"
    "nodejs-12.22.12"
  ];
}
