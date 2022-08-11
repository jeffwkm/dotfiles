{ config, lib, pkgs, ... }: {
  imports = [ ../modules/nix-config.nix ];

  nix.trustedUsers = [ "@admin" ];
  users.nix.configureBuildUsers = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  environment.systemPackages = with pkgs; [ vim babashka ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  # Install and setup ZSH to work with nix(-darwin) as well
  programs.zsh.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
