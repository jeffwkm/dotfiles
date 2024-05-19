{ lib, config, ... }:
let
  inherit (lib.my) mkOpt mkBoolOpt;
  inherit (config) user host;
in {
  options = with lib.types; {
    user = {
      name = mkOpt str "jeff";
      full-name = mkOpt str "Jeff Workman";
      email = mkOpt str "jeff.workman@protonmail.com";
      home = mkOpt str "${host.home-root}/${user.name}";
    };
    host = {
      name = mkOpt (nullOr str) null;
      darwin = mkBoolOpt false;
      gui = mkBoolOpt false;
      minimal = mkBoolOpt false;
      config-dir = mkOpt str "${user.home}/.config/nixpkgs";
      home-root = mkOpt str (if host.darwin then "/Users" else "/home");
      optimize = mkBoolOpt false;
      fd = { ignores = mkOpt lines [ ]; };
    };
  };
}
