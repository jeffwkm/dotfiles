{ config, lib, pkgs, ... }: {
  nix.settings.trusted-users = [ "root" "jeff" ];
  environment.pathsToLink = [ "/libexec" ];
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/New_York";
  nixpkgs.config.allowBroken = false;

  system.autoUpgrade = {
    enable = false;
    allowReboot = false;
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
    earlySetup = true;
  };

  # Increase open file limits (fixes crash from sway)
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";
  security.pam.loginLimits = [{
    domain = "*";
    type = "hard";
    item = "nofile";
    value = "1048576";
  }];

  programs.zsh = {
    enable = true;
    # enableCompletion = false;
    # enableBashCompletion = false;
  };
  users.defaultUserShell = pkgs.zsh;
  environment.shells = [ pkgs.zsh ];

  programs.fuse = { userAllowOther = true; };
  programs.dconf.enable = true;

  users.extraUsers.jeff = {
    isNormalUser = true;
    home = "/home/jeff";
    description = "Jeff Workman";
    extraGroups =
      [ "audio" "input" "users" "wheel" "video" "docker" "libvirtd" ];
    uid = 1000;
  };
  users.extraUsers.root = {
    extraGroups = [ "audio" "input" "plugdev" "libvirtd" ];
  };
  users.users.jeff.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDrJoknvHUr9pMTK+ppRf9t0XsfQTysKBNfXo/Dm9BRrIy6NIKn47xGUr2QLKrgHtZcY7aLHp4EUpAiyXfbXuF5PFtht87blmJ12cn1gjyZHANgKY9oEJja6egSEbMpoHjpdb3ujCorfAqFs9WEHwdIoNWjRb1nGWGekISCj8OpBEtUjMwbRf3H1CK9HCPFq6xUtLWtq4vdyTnMWdOnwlgw0Kh2hA3qWmg0/kordoS1DC3lXqlHLPj4nwOgxczwqJvcJa4Rvq5afsIavK7nhtmhN2do1YSfY7+OvE0fzesf94/qzz1UJaLn6MHMXnP9GTsJ63zUtEXvk4spekbnee4b jeff@jeff-desktop"
  ];

  security.sudo = { wheelNeedsPassword = false; };

  programs.ssh = {
    extraConfig = ''
      ServerAliveInterval 30
      ServerAliveCountMax 3
      ExitOnForwardFailure yes'';
  };

  services.openssh = {
    enable = true;
    ports = [ 22 ];
    settings.X11Forwarding = true;
  };

  services.printing = { enable = config.local.printing; };

  programs.mosh.enable = true;

  virtualisation.docker = { enable = config.local.docker; };

  xdg.mime.enable = true;

  environment.systemPackages = with pkgs;
    [
      binutils
      coreutils
      curl
      file
      inetutils
      kmon
      libtool
      lsof
      nix-index # :: A files database for nixpkgs
      nix-prefetch-scripts
      openssh
      openssl
      patchelf
      pinentry
      pinentry-curses
      pkg-config
      psmisc # :: killall, etc.
      readline
      screen
      sshfs
      tmux
      vim
      wget
      xdg-utils
    ] ++ lib.lists.optionals config.local.docker [ docker docker-compose ]
    ++ lib.lists.optionals (!config.local.cloud) [
      smartmontools
      lm_sensors
      acpi
      exfat
      kernelshark
      pciutils
      linuxPackages.cpupower
      glib
      exfat
    ];
}
