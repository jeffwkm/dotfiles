{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user host modules;
  cfg = config.modules.aws;
in {
  options.modules.aws = { enable = mkBoolOpt modules.dev.enable-all; };
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.sessionVariables = {
        AWS_VAULT_BACKEND = "pass";
        AWS_VAULT_PASS_CMD = "pass";
        AWS_VAULT_PASS_PASSWORD_STORE_DIR = "${user.home}/.password-store";
        AWS_VAULT_PASS_PREFIX = "awsvault";
      };
      home.packages = with pkgs; [ awscli2 aws-vault ec2-api-tools pass ];
    };
  };
}
