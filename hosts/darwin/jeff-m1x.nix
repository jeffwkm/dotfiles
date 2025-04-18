{ config, lib, pkgs, ... }: {
  config = {
    host.darwin = true;

    modules = {
      dev.enable = true;
      dev.rust.enable = true;
      dev.jdk.enable = true;
      dev.clojure.enable = true;
      dev.extra.enable = true;
      dev.aws.enable = true;
      fonts.enable = true;
      programs.mpv.enable = true;
      programs.firefox.enable = false;
      programs.spotify.enable = true;
      programs.vscode.enable = true;
      programs.kitty.enable = true;
    };

    environment.systemPackages = with pkgs; [ sshfs cava ];

    networking.computerName = "Jeff-M1X";
    networking.knownNetworkServices = [ "Wi-Fi" "USB 10/100/1000 LAN" ];

    system.stateVersion = 4;
  };
}
