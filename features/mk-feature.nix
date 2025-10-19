{
  name,
  options ? { },
  classOptions ? { },
  nixos ? null,
  homeManager ? null,
  darwin ? null,
}:
let
  module =
    { config, lib, ... }:
    let
      opts = {
        ordenada.features.${name} = {
          enable = lib.mkEnableOption "the ${name} feature";
        } // options;
      };
      mkClassConfig =
        class: classModule:
        lib.optionalAttrs (classModule != null) {
          ${class} =
            args@{ config, pkgs, ... }:
            {
              options = lib.recursiveUpdate opts (
                lib.optionalAttrs (builtins.hasAttr class classOptions) {
                  ordenada.features.${name} = classOptions.${class};
                }
              );
              config = lib.mkIf config.ordenada.features.${name}.enable (
                if builtins.isFunction classModule then classModule args else classModule
              );
            };
        };
    in
    {
      config.ordenada.modules.${name} =
        mkClassConfig "nixos" nixos
        // mkClassConfig "homeManager" homeManager
        // mkClassConfig "darwin" darwin;
    };
in
{
  imports = [ module ];
}
