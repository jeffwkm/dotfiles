{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let inherit (config) user host;
in {
  config = {
    home-manager.users.${user.name} = {
      home.packages = with pkgs; [ gh ];

      programs.git = {
        enable = true;
        package = optimize config pkgs.git;
        userName = "${config.user.full-name}";
        userEmail = "${config.user.email}";
        extraConfig = {
          push.default = "current";
          pull.rebase = true;
          init.defaultBranch = "main";
          url."https://aur.archlinux.org/".insteadOf = "aur://";
          url."git@github.com:".insteadOf = "gh://";
          url."git@bitbucket.org:".insteadOf = "bb://";
        };
      };
    };
  };
}
