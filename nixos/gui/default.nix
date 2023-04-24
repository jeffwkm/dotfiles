{ config, lib, pkgs, ... }:
let optimize = config.util.optimizeDefault;
in {
  imports = [ ./fonts.nix ./hyprland.nix ./sway.nix ];
  environment.systemPackages = with pkgs; [ pinentry-gtk2 ];
  nixpkgs.overlays = [
    (self: super: {
      wl-clipboard-x11 = super.stdenv.mkDerivation rec {
        pname = "wl-clipboard-x11";
        version = "5";

        src = super.fetchFromGitHub {
          owner = "brunelli";
          repo = "wl-clipboard-x11";
          rev = "v${version}";
          sha256 = "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb";
        };

        dontBuild = true;
        dontConfigure = true;
        propagatedBuildInputs = [ super.wl-clipboard ];
        makeFlags = [ "PREFIX=$(out)" ];
      };

      xsel = self.wl-clipboard-x11;
      xclip = self.wl-clipboard-x11;
    })
    (final: prev: {
      mpv = optimize (prev.wrapMpv
        (prev.mpv-unwrapped.override { vapoursynthSupport = true; }) {
                                         scripts = with final.mpvScripts; [
                                           autoload
                                           convert
                                           mpris
                                           # mpvacious
                                           mpv-playlistmanager
                                           # sponsorblock
                                           # thumbnail
                                           # unstable.mpvScripts.youtube-quality
                                         ];
                                         extraMakeWrapperArgs = [
                                           "--prefix"
                                           "LD_LIBRARY_PATH:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
                                         ];
                                       });
    })
  ];
  nixpkgs.config.packageOverrides = pkgs: {
    wofi = optimize pkgs.wofi;
    alacritty = optimize pkgs.alacritty;
    waybar = optimize pkgs.waybar;
  };
}
