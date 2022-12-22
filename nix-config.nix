{ config, lib, pkgs, ... }: {
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';
  nixpkgs.config.allowUnfree = true;
}
