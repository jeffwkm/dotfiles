{ config, lib, pkgs, ... }: {
  xdg.configFile = {
    "gtkrc-2.0".source = ../../dotfiles/gtkrc-2.0;
    "gtk-2.0/gtkfilechooser.ini".source =
      ../../dotfiles/gtk-2.0/gtkfilechooser.ini;
    "gtk-3.0/settings.ini".source = ../../dotfiles/gtk-3.0/settings.ini;
  };
  home.file = {
    ".Xdefaults".source = ../../dotfiles/Xdefaults;
    ".Xresources.d/".source = ../../dotfiles/Xresources.d;
    ".Xresources".source = ../../dotfiles/Xresources.d/.Xresources.gruvbox-dark;
    ".looking-glass-client.ini".source =
      ../../dotfiles/looking-glass-client.ini;
  };

  imports = [
    ./alacritty.nix
    ./mako.nix
    ./mpv.nix
    ./sway.nix
    ./themes.nix
    ./waybar.nix
    ./wayland.nix
    ./wofi.nix
  ];

  home.packages = with pkgs; [
    # lxappearance
    # motion # :: Monitors the video signal from cameras
    # nvidia-optical-flow-sdk # Nvidia optical flow headers for computing the relative motion of pixels between images
    # obs-wlrobs # :: An obs-studio plugin that allows you to screen capture on wlroots based wayland compositors
    # unstable.obs-studio-plugins.looking-glass-obs
    alacritty
    baobab
    chromedriver
    chromium
    egl-wayland
    firefox-wayland
    fontconfig
    glpaper # :: Wallpaper program for wlroots based Wayland compositors such as sway that allows you to render glsl shaders as your wallpaper
    gnome-podcasts
    gnome-usage
    gnome.dconf-editor
    gnome.eog
    gnome.evince
    gnome.file-roller
    gnome.gedit
    gnome.nautilus
    gnome.sushi
    gobject-introspection
    google-chrome
    grim
    gthumb
    gtk3
    gtkperf
    lxappearance-gtk2
    mesa-demos
    obs-studio
    pass-wayland # :: password-store
    pavucontrol # :: GTK program to control pulseaudio
    polkit_gnome
    # qt5.full
    # qt5.qtwayland
    # shotwell
    snes9x-gtk
    sov # :: An overlay that shows schemas for all workspaces to make navigation in sway easier
    spotify # :: Play music from the Spotify music service
    spotifywm # :: Wrapper around Spotify that correctly sets class name before opening the window
    swappy # :: A Wayland native snapshot editing tool, inspired by Snappy on macOS
    sway-contrib.grimshot # :: A helper for screenshots within sway
    # swayest-workstyle # :: Map sway workspace names to icons defined depending on the windows inside of the workspace
    looking-glass-client
    scream
    virt-manager
    wayland
    wayland-protocols
    wayland-utils
    wayshot # :: A native, blazing-fast screenshot tool for wlroots based compositors such as sway and river
    wev
    wf-recorder
    wl-clipboard
    wlr-randr
    wtype
    xorg.xev
    xorg.xkbcomp
    xorg.xkill
    xorg.xprop
    xorg.xrandr
    xorg.xrdb
    xorg.xset
    xorg.xwininfo
    xterm
    ydotool
  ];

  qt = {
    enable = true;
    platformTheme = "gnome";
    style.name = "adwaita-dark";
    style.package = pkgs.adwaita-qt;
  };

  services.spotifyd.enable = true;
}
