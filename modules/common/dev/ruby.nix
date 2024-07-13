{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) user modules;
  inherit (modules) dev;
  rubyPackages = (ps:
    with ps; [
      curses
      ffi
      glib2
      gtk3
      native-package-installer
      pango
      pkg-config
      rake
      rexml
      sequel
      sqlite3
      terminal-table
      ## repl (bundler console)
      minitest
      mutex_m
      pry
      simplecov
    ]);
  rubyCustom = pkgs.pkgs-stable.ruby_3_3.withPackages rubyPackages;
  cfg = config.modules.dev.ruby;
in {
  options.modules.dev.ruby.enable = mkBoolOpt dev.enable-all;

  config = {
    environment.systemPackages =
      if cfg.enable then [ (optimize config rubyCustom) ] else [ pkgs.ruby ];
    home-manager.users.${user.name} = { };
  };
}
