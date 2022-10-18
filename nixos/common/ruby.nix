{ config, lib, pkgs, ... }:
let
  ruby-packages = (ps:
    with ps; [
      ffi
      gio2
      glib2
      sqlite3
      pkg-config
      rake
      native-package-installer
      pango
      curses
    ]);
  ruby-custom = pkgs.ruby_2_7.withPackages ruby-packages;
in { environment.systemPackages = with pkgs; [ ruby-custom ]; }
