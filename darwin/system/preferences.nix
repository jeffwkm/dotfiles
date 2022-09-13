{ config, pkgs, ... }: {

  system = {

    keyboard.enableKeyMapping = true;

    defaults = {
      loginwindow = {
        GuestEnabled = false;
        SHOWFULLNAME = false;
      };

      spaces.spans-displays = false;

      finder = {
        CreateDesktop = false;
        AppleShowAllExtensions = true;
        AppleShowAllFiles = false;
        ShowStatusBar = true;
        ShowPathbar = true;
        FXDefaultSearchScope = "SCcf";
        FXPreferredViewStyle = "Nlsv";
        FXEnableExtensionChangeWarning = true;
        _FXShowPosixPathInTitle = true;
      };

      smb = {
        # NetBIOSName = "jeff-m1x";
        # ServerDescription = "jeff-m1x";
      };

      # trackpad settings
      # trackpad = {
      #   # silent clicking = 0, default = 1
      #   ActuationStrength = 1;
      #   # enable tap to click
      #   Clicking = true;
      #   # enable tap-to-drag
      #   Dragging = true;
      #   # firmness level, 0 = lightest, 2 = heaviest
      #   FirstClickThreshold = 1;
      #   # firmness level for force touch
      #   SecondClickThreshold = 1;
      #   # don't allow positional right click
      #   TrackpadRightClick = false;
      #   # three finger drag for space switching
      #   TrackpadThreeFingerDrag = true;
      # };

      # firewall settings
      alf = {
        # 0 = disabled 1 = enabled 2 = blocks all connections except for essential services
        globalstate = 1;
        loggingenabled = 0;
        stealthenabled = 1;
      };

      # Whether to enable quarantine for downloaded applications. The default is true.
      LaunchServices.LSQuarantine = false;

      # dock settings
      dock = {
        autohide = true;
        autohide-delay = 0.0;
        autohide-time-modifier = 0.5;
        expose-animation-duration = 0.5;
        expose-group-by-app = true;
        tilesize = 40;
        static-only = false;
        # Whether to make icons of hidden applications tranclucent. The default is false.
        showhidden = true;
        show-recents = false;
        show-process-indicators = true;
        orientation = "bottom";
        mru-spaces = false;
        launchanim = true;
        mineffect = "genie";
        minimize-to-application = false;
      };

      NSGlobalDomain = {
        "com.apple.sound.beep.feedback" = 0;
        "com.apple.sound.beep.volume" = 0.0;
        # ApplePressAndHoldEnabled = false;
        # InitialKeyRepeat = 10;
        # KeyRepeat = 0;
        AppleShowAllExtensions = true;
        AppleShowScrollBars = "Automatic";
      };
    };

    keyboard = {
      remapCapsLockToControl = true;
      # userKeyMapping = [];
    };

  };
}
