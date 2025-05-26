{ lib, ... }:

{
  mkEnableTrueOption =
    name:
    lib.mkOption {
      default = true;
      example = true;
      description = "Whether to enable ${name}.";
      type = lib.types.bool;
    };
    ## TODO: Implement `setGlobal` here
}
