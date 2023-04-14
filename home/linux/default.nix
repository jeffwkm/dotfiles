{ config, lib, pkgs, ... }:
let optimize = config.util.optimizeDefault;
in {
  imports = [ ../zsh/linux.nix ./ssh-agent.nix ./gpg-agent.nix ];

  xdg.configFile."mpv/mpv.conf".text =
    (builtins.readFile ../../dotfiles/mpv/mpv.conf) + ''
      profile=gpu-hq
      gpu-context=wayland
      vf=format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
    '';

  xdg.configFile."mpv/input.conf".text =
    (builtins.readFile ../../dotfiles/mpv/input.conf) + ''
      I vf toggle format=yuv420p,vapoursynth=~~/motioninterpolation.vpy:8:12
    '';

  home.sessionPath = [ ];

  home.packages = with pkgs; [
    (optimize fd)
    (optimize lsd)
    # caerbannog # :: Mobile-friendly Gtk frontend for password-store
    ddcutil # :: shell utility for controlling displays (DDC)
    ffmpegthumbnailer
    inxi
    iotop
    latencytop
    libinput
    libnotify
    mkvtoolnix-cli # :: Cross-platform tools for Matroska (needed for svpcode)
    ncpamixer # :: full ncurses interface to pamixer
    pamixer # :: shell utility for controlling pulseaudio
    playerctl # :: shell utility for controlling media players (MPRIS)
    podgrab # :: A self-hosted podcast manager to download episodes as soon as they become live
    screenfetch # :: Fetches system/theme information in terminal for Linux desktop screenshots
    spotdl # :: Download your Spotify playlists and songs along with album art and metadata
    spotify-cli-linux # :: A command line interface to Spotify on Linux
    usbutils
    xdg-user-dirs
  ];

  systemd.user.services.emacs = {
    Unit = {
      Description = "Emacs daemon";
      After = [ "default.target" ];
    };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      Type = "simple";
      ExecStart = "/run/current-system/sw/bin/emacs --fg-daemon";
      Restart = "no";
      Environment = [
        "DOOMDIR=%h/.config/doom-config"
        "DOOMLOCALDIR=%h/.config/doom-local"
      ] ++ lib.optional (!config.home.local.emacs.install)
        "LD_LIBRARY_PATH=${pkgs.freetype_subpixel}/lib";
    };
  };

  programs.git.package = (optimize pkgs.git);

  # xdg.configFile."dconf/user".source = ../../dotfiles/dconf/user;
}
