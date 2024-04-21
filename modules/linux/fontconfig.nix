{ config, lib, ... }:
with lib;
let inherit (config) modules;
in {
  config = {
    fonts = mkIf modules.fonts.enable {
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
  };
}
