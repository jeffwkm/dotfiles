{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.vfio.looking-glass;
  pwd = "${host.config-dir}/modules/linux/vfio";
in {
  options.modules.vfio.looking-glass = {
    enable = mkBoolOpt modules.vfio.enable;
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = { config, pkgs, ... }:
      let link = config.lib.file.mkOutOfStoreSymlink;
      in {
        home.file.".looking-glass-client.ini".source =
          link "${pwd}/looking-glass-client.ini";

        home.packages = with pkgs;
          [
            looking-glass-client
            # (looking-glass-client.overrideAttrs (old: {
            #   src = fetchFromGitHub {
            #     owner = "gnif";
            #     repo = "LookingGlass";
            #     rev = "d060e375ea47e4ca38894ea7bf02a85dbe29b1f8";
            #     hash = "sha256-DuCznF2b3kbt6OfoOUD3ijJ1im7anxj25/xcQnIVnWc=";
            #     fetchSubmodules = true;
            #   };
            #   patches = [ ];
            # }))
          ];
      };
  };
}
