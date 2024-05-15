{ config, lib, ... }:
with lib;
let inherit (lib.my) mkOpt;
in {
  options.theme = with types; {
    monoFamily = mkOpt str "JetBrainsMono Nerd Font";
    monoFamilyAlt = mkOpt str "JetBrains Mono";
    monoStyle = mkOpt str "Semibold";
    monoSize = mkOpt number 10;
    windowOpacity = mkOpt float 0.85;
    # "#181a20"
    colors = with types; {
      # background = mkOpt str "#191a1e";
      background = mkOpt str "#24273a";
    };
  };
}
