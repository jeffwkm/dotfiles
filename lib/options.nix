{ lib, ... }:
let inherit (lib) mkOption types;
in {
  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default:
    mkOption {
      inherit default;
      type = types.bool;
      example = true;
    };

  mkBoolOpt' = default: description:
    mkOption {
      inherit default description;
      type = types.bool;
      example = true;
    };
}
