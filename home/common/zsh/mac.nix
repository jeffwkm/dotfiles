{ config, lib, pkgs, ... }:
let cfg = (import ./config.nix {
      extra = {
        shellAliases = {
          brew = "/opt/homebrew/bin/brew";
          ibrew = "arch -x86_64 /usr/local/homebrew/bin/brew";
          mv = "/bin/mv";
        };
        initExtraFirst = "";
        initExtra = ''
          # add homebrew bin to PATH
          if [[ ! $PATH =~ ".*homebrew/bin.*" ]] ; then
            export PATH="/opt/homebrew/bin:$PATH"
          fi

          if [ -z $DISPLAY ]; then export DISPLAY=:0.0; fi
        '';
      };
    }).cfg;
in {
  programs.zsh = cfg;
}
