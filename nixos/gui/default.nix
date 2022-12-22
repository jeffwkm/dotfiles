{ config, lib, pkgs, ... }: {
  imports =
    [ ./freetype.nix ./fonts.nix ./hyprland.nix ./redshift.nix ./sway.nix ];
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
      mpv = prev.mpv.override {
        scripts = with final.mpvScripts; [
          # autoload
          convert
          mpris
          mpvacious
          mpv-playlistmanager
          # sponsorblock
          # thumbnail
          # unstable.mpvScripts.youtube-quality
        ];
      };
    })
  ];
  nixpkgs.config.packageOverrides = pkgs: rec {
    wofi = (config.util.optimizeDefault pkgs.wofi);
    alacritty = (config.util.optimizeDefault pkgs.alacritty);
    waybar = (config.util.optimizeDefault (pkgs.waybar.overrideAttrs
      (prev: { mesonFlags = prev.mesonFlags ++ [ "-Dexperimental=true" ]; })));
  };
}
