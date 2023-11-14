{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  cfg = config.modules.linux;
in {
  options.modules.linux = { systemd-boot = mkBoolOpt false; };

  config = {
    nixpkgs.config.permittedInsecurePackages =
      [ "nodejs-10.24.1" "nodejs-12.22.12" "python-2.7.18.6" ];

    nix.settings.trusted-users = [ "root" "${user.name}" ];
    environment.pathsToLink = [ "/libexec" ];

    environment.sessionVariables = {
      SSH_AUTH_SOCK = "/run/user/1000/ssh-agent.socket";
      _GLOBAL_ENV_LOADED = "1";
    };

    xdg.mime.defaultApplications = mkIf modules.programs.chromium.enable {
      "text/html" = "chromium.desktop";
      "x-scheme-handler/http" = "chromium.desktop";
      "x-scheme-handler/https" = "chromium.desktop";
      "x-scheme-handler/about" = "chromium.desktop";
      "x-scheme-handler/unknown" = "chromium.desktop";
    };

    services.vscode-server.enable = modules.programs.vscode.enable;

    users.defaultUserShell = mkIf modules.zsh.enable pkgs.zsh;

    fonts = mkIf modules.fonts.enable {
      enableDefaultPackages = true;
      fontDir.enable = true;
      fontDir.decompressFonts = true;
      fontconfig = {
        enable = true;
        antialias = true;
        hinting.enable = true;
        subpixel.rgba = "rgb";
        subpixel.lcdfilter = "default";
        defaultFonts = {
          serif = [ "Inter:medium" "Inter" "Noto Sans" ];
          sansSerif = [ "Inter:medium" "Inter" "Noto Sans" ];
          monospace = [
            "JetBrains Mono Nerd Font:medium"
            "JetBrains Mono Nerd Font"
            "JetBrains Mono:medium"
            "JetBrains Mono"
          ];
        };
        localConf = ''
          <?xml version="1.0"?>
          <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
          <fontconfig>
            <match target="pattern">
              <test qual="any" name="family"><string>Noto Sans</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Roboto</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Segoe UI</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>arial</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Helvetica Neue</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Helvetica</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Liberation Sans</string></test>
              <edit name="family" mode="prepend" binding="same"><string>Inter</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>ui-monospace</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>SFMono-Regular</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>SF Mono</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>SF Mono</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Menlo</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>

            <match target="pattern">
              <test qual="any" name="family"><string>Consolas</string></test>
              <edit name="family" mode="prepend" binding="same"><string>JetBrainsMono Nerd Font</string></edit>
            </match>
          </fontconfig>
        '';
      };
    };

    environment.systemPackages = with pkgs; [
      binutils
      coreutils
      curl
      file
      fscrypt-experimental
      inetutils
      iotop
      kmon
      libtool
      lsof
      nix-index
      openssh
      openssl
      pinentry
      pinentry-curses
      pkg-config
      psmisc
      readline
      sshfs
      tmux
      wget
      xdg-utils
    ];

    security.sudo = { wheelNeedsPassword = false; };

    users.extraUsers.${user.name} = {
      isNormalUser = true;
      home = "${user.home}";
      description = "${user.full-name}";
      extraGroups =
        [ "audio" "input" "users" "wheel" "video" "docker" "libvirtd" ];
      uid = 1000;
    };

    users.users.${user.name}.openssh.authorizedKeys.keys =
      [ (builtins.readFile ../../dotfiles/id_rsa.pub) ];

    system.autoUpgrade = {
      enable = false;
      allowReboot = false;
    };

    # Increase open file limits
    # Fixes various "too many open files" errors
    systemd.extraConfig = "DefaultLimitNOFILE=1048576";
    security.pam.loginLimits = [{
      domain = "*";
      type = "hard";
      item = "nofile";
      value = "1048576";
    }];
    services.logind.extraConfig = ''
      # don’t shutdown when power button is short-pressed
      HandlePowerKey=ignore
    '';

    xdg.mime.enable = true;

    programs.fuse = { userAllowOther = true; };
    programs.dconf.enable = true;

    services.openssh = {
      enable = true;
      ports = [ 22 ];
      settings.X11Forwarding = true;
    };

    services.dbus.enable = true;

    programs.mosh.enable = true;

    boot.kernel.sysctl = {
      "fs.inotify.max_user_instances" = 8192;
      "fs.inotify.max_user_watches" = 1164444;
    };

    networking.firewall.allowedTCPPorts = [ 445 139 ]
      ++ (if options.modules.programs.spotify then [ 57621 ] else [ ]);
    networking.firewall.allowedUDPPorts = [ 137 138 ]
      ++ (if options.modules.programs.spotify then [ 5353 ] else [ ]);

    boot.loader = mkIf cfg.systemd-boot {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      systemd-boot = {
        enable = true;
        consoleMode = "auto";
        configurationLimit = 50;
      };
    };
  };
}
