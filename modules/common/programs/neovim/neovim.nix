{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  pwd = "${host.config-dir}/modules/common/programs/neovim";
  cfg = modules.programs.neovim;
in {
  options.modules.programs.neovim.enable = mkBoolOpt true;

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }: {
      programs.neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
        plugins = with pkgs.vimPlugins; [ nvim-treesitter.withAllGrammars ];
      };

      xdg.configFile."nvim/".source =
        config.lib.file.mkOutOfStoreSymlink "${pwd}/config";
    };
  };
}
