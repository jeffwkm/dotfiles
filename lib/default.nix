{ inputs, lib, ... }:
let
  inherit (lib) makeExtensible attrValues foldr;
  inherit (modules) mapModules;

  # bootstrap modules.nix for loading the rest of the modules
  modules = import ./modules.nix {
    inherit lib;
    self.attrs = import ./attrs.nix {
      inherit lib;
      self = { };
    };
  };

  mylib = makeExtensible
    (self: mapModules ./. (file: import file { inherit self lib inputs; }));
in mylib.extend (final: prev: foldr (a: b: a // b) { } (attrValues prev))
