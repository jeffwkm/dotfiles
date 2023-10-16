{ config, lib, pkgs, ... }: {
  config = {
    modules = {
      dev.enable = true;
      dev.rust.enable = true;
      dev.jdk.enable = true;
      dev.clojure.enable = true;
      aws.enable = true;
      fonts.enable = true;
      emacs.enable = true;
      emacs.install = true;
      programs.mpv.enable = true;
      programs.spotify.enable = true;
      programs.vscode.enable = true;
      programs.alacritty = {
        enable = true;
        fontSize = "14";
        fontSizeLarge = "16";
        fontSizeHuge = "18";
        decorations = "buttonless";
        fontFamily = "JetBrainsMono Nerd Font";
        fontStyle = "Medium";
        # opacity = "0.9";
      };
    };

    networking.computerName = "Jeff-M1X";
    networking.knownNetworkServices = [ "Wi-Fi" "USB 10/100/1000 LAN" ];

    system.stateVersion = 4;
  };
}
