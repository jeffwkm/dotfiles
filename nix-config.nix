{ config, lib, pkgs, ... }: {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.auto-optimise-store = true;
  nix.settings.keep-outputs = true;
  nix.settings.keep-derivations = true;
  nix.settings.extra-platforms =
    lib.optional (pkgs.system == "aarch64-darwin") [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
  nixpkgs.config.allowUnfree = true;
}
