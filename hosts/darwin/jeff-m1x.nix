{ config, lib, pkgs, ... }: {
  config = {
    modules = {
      dev.enable = true;
      dev.rust.enable = true;
      dev.jdk.enable = true;
      dev.clojure.enable = true;
      fonts.enable = true;
      emacs.enable = true;
      emacs.install = false;
      programs.mpv.enable = true;
      programs.spotify.enable = true;
      programs.alacritty = {
        enable = true;
        fontSize = "14";
        fontSizeLarge = "16";
        fontSizeHuge = "18";
        decorations = "buttonless";
        fontFamily = "JetBrainsMono Nerd Font";
        fontStyle = "Medium";
      };
    };

    networking.computerName = "Jeff-M1X";
    networking.knownNetworkServices = [ "Wi-Fi" "USB 10/100/1000 LAN" ];

    system.stateVersion = 4;
  };
}
