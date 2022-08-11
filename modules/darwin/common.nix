{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) programs;
in {
  options = {
    # copy linux options needed for build
    modules.desktop = { enable = mkBoolOpt false; };
    modules.wayland = { enable = mkBoolOpt false; };
  };

  config = {
    nixpkgs.config.permittedInsecurePackages =
      [ "nodejs-10.24.1" "nodejs-12.22.12" "python-2.7.18.6" ];

    nix.settings.trusted-users = [ "@admin" ];
    nix.configureBuildUsers = true;
    services.nix-daemon.enable = true;

    programs.bash.enable = true;
    environment.loginShell = pkgs.zsh;
    environment.systemPackages = with pkgs; [ vim babashka ];

    homebrew.brews = optionals programs.mpv.enable [ "mpv" "vapoursynth" ];
    homebrew.casks = with programs;
      optional spotify.enable "spotify"
      ++ optional vscode.enable "visual-studio-code"
      ++ optional firefox.enable "firefox"
      ++ optional alacritty.enable "alacritty";
    homebrew.extraConfig = mkIf modules.emacs.enable ''
      brew "railwaycat/emacsmacport/emacs-mac", args: ["with-native-compilation", "with-emacs-big-sur-icon", "with-librsvg", "with-no-title-bars", "with-mac-metal" ]
      brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-native-comp", "with-modern-paper-icon", "with-no-titlebar-and-round-corners"]
      # brew "d12frosted/emacs-plus/emacs-plus@29", args: ["with-native-comp", "with-poll", "with-modern-paper-icon", "with-no-titlebar-and-round-corners", "with-no-frame-refocus"]
      # brew "d12frosted/emacs-plus/emacs-plus@30", args: ["with-native-comp", "with-modern-paper-icon", "with-no-frame-refocus", "with-poll"]
    '';

    # Set of files to be linked in '/Library/LaunchAgents'
    environment.launchAgents = { };
    # Set of files to be linked in '/Library/LaunchDaemons'
    environment.launchDaemons = {
      # ./com.jeff.pre-login.plist
    };
    # Set of files to be linked in '~/Library/LaunchDaemons'
    environment.userLaunchAgents = { };

    environment.etc = mkIf false {
      "pf.conf" = ''
        #
        # Default PF configuration file.
        #
        # This file contains the main ruleset, which gets automatically loaded
        # at startup.  PF will not be automatically enabled, however.  Instead,
        # each component which utilizes PF is responsible for enabling and disabling
        # PF via -E and -X as documented in pfctl(8).  That will ensure that PF
        # is disabled only when the last enable reference is released.
        #
        # Care must be taken to ensure that the main ruleset does not get flushed,
        # as the nested anchors rely on the anchor point defined here. In addition,
        # to the anchors loaded by this file, some system services would dynamically
        # insert anchors into the main ruleset. These anchors will be added only when
        # the system service is used and would removed on termination of the service.
        #
        # See pf.conf(5) for syntax.
        #

        #
        # com.apple anchor point
        #
        scrub-anchor "com.apple/*"
        nat-anchor "com.apple/*"
        rdr-anchor "com.apple/*"
        rdr-anchor "port80"
        dummynet-anchor "com.apple/*"
        anchor "com.apple/*"
        load anchor "com.apple" from "/etc/pf.anchors/com.apple"
        load anchor "port80" from "/etc/pf.anchors/port80"
      '';

      "pf.anchors/port80" = ''
        rdr pass on lo0 inet proto tcp from any to 127.0.0.1 port = 80 -> 127.0.0.1 port 8080
      '';
    };

    home-manager.users.${user.name} = { config, pkgs, ... }: {
      home.packages = with pkgs; [
        (lowPrio coreutils-full)
        fontconfig
        gnugrep
        gnused
      ];
    };
  };
}
