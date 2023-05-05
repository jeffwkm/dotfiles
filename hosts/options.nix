{ lib, config, ... }:
with lib;
with lib.my;
with types;
let inherit (config) user host;
in {
  options = {
    user = {
      name = mkOpt str "jeff";
      full-name = mkOpt str "Jeff Workman";
      email = mkOpt str "jeff.workman@gmail.com";
      home = mkOpt str "${host.home-root}/${user.name}";
    };
    host = {
      name = mkOpt (nullOr str) null;
      darwin = mkBoolOpt false;
      config-dir = mkOpt str "${user.home}/.config/nixpkgs";
      home-root = mkOpt str (if host.darwin then "/Users" else "/home");
      optimize = mkBoolOpt false;
      fd = { ignores = mkOpt lines [ ]; };
    };
  };
}
