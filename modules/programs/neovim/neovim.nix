{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  pwd = "${host.config-dir}/modules/programs/neovim";
  cfg = modules.programs.neovim;
in {
  options = { modules.programs.neovim.enable = mkBoolOpt true; };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      programs.neovim.enable = true;
      programs.neovim.viAlias = true;
      programs.neovim.vimAlias = true;

      xdg.configFile."nvim/".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/config";
    };
  };
}
