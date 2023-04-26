{ config, lib, pkgs, ... }: {
  nix.settings.substituters =
    [ "https://cache.nixos.org/" "https://nix-community.cachix.org" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  imports = [ ../nix-config.nix ];

  nix.settings.trusted-users = [ "@admin" ];
  nix.configureBuildUsers = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  environment.systemPackages = with pkgs; [ vim babashka ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  # Install and setup ZSH to work with nix(-darwin) as well
  programs.zsh.enable = true;
  programs.zsh.enableCompletion = false;
  programs.zsh.enableBashCompletion = false;
  programs.zsh.promptInit = "";

  nixpkgs.config.packageOverrides = pkgs: {
    oraclejdk = pkgs.openjdk8;
    jdk = pkgs.openjdk8;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
