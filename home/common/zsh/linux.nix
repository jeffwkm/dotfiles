{ config, lib, pkgs, ... }:
let cfg = (import ./config.nix {
      extra = {
        shellAliases = {
          iotop = "sudo iotop";
          virsh = "VISUAL='emacs -nw' sudo -E virsh";
          ### nixos
          update-nix = "sudo nix-channel --update";
          "@un" = "update-nix";
          switch-nix = "sudo nixos-rebuild switch";
          "@n" = "switch-nix";
          upgrade-nix = "update-nix && switch-nix";
          "@N" = "upgrade-nix";
          ### home-manager
          update-home = "nix-channel --update";
          "@uh" = "update-home";
          switch-home = "home-manager switch && source ~/.zshrc";
          "@h" = "switch-home";
          upgrade-home = "update-home && switch-home";
          "@H" = "upgrade-home";
          ### nixos + home-manager
          update-both = "update-nix && update-home";
          "@U" = "update-both";
          switch-both = "switch-nix && switch-home";
          "@S" = "switch-both";
          upgrade-both = "update-both && switch-both";
          "@SS" = "upgrade-both";
          ### systemctl
          sd = "sudo systemctl";
          sds = "sd status";
          sdr = "sd restart";
          ### systemctl --user
          sdu = "systemctl --user";
          sdus = "sdu status";
          sdur = "sdu restart";
          ### systemctl daemon-reload
          sdd = ''
            echo -n 'daemon-reload:'
            sudo systemctl daemon-reload
            echo -n ' [system]'
            systemctl --user daemon-reload
            echo -n ' [user]'
            echo'';
          ### cpupower
          cpinfo = "sudo cpupower frequency-info";
          cpgov = "sudo cpupower frequency-set -g";
          cpgovperf = "cpgov performance";
          cpgovsave = "cpgov powersave";
          ### zsh-limit
          "@c" = "zsh-limit compile";
          "@2" = "zsh-limit cpu-half";
          "@3" = "zsh-limit cpu-third";
          "@4" = "zsh-limit cpu-quarter";
        };
        initExtraFirst = ''
          export NIXOS=/etc/nixos
        '';
        initExtra = ''
          function reload_gtk_theme() {
            theme=$(gsettings get org.gnome.desktop.interface gtk-theme)
            gsettings set org.gnome.desktop.interface gtk-theme ${"''"}
            sleep 1
            gsettings set org.gnome.desktop.interface gtk-theme $theme
          }
        '';
      };
    }).cfg;
in {
  programs.zsh = cfg;
}
