args@{
  name,
  options ? { },
  classOptions ? { },
  ...
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
        class: module: extraModules:
        lib.optionalAttrs (module != null) {
          ${class} =
            args@{ config, pkgs, ... }:
            {
              imports = extraModules;
              options = lib.recursiveUpdate opts (
                lib.optionalAttrs (builtins.hasAttr class classOptions) {
                  ordenada.features.${name} =
                    if builtins.isFunction classOptions.${class} then
                      classOptions.${class} args
                    else
                      classOptions.${class};
                }
              );
              config = lib.mkIf config.ordenada.features.${name}.enable (
                if builtins.isFunction module then module args else module
              );
            };
        };
    in
    {
      config = lib.mkMerge (
        lib.mapAttrsToList
          (class: module: {
            ordenada.modules.${name} = mkClassConfig class module (
              lib.optionals (class == "nixos" || class == "darwin") [
                (
                  { config, ... }:
                  {
                    config = lib.mkIf config.ordenada.features.home.enable {
                      home-manager.sharedModules = [
                        {
                          ordenada.features.${name}.enable = lib.mkDefault true;
                        }
                      ];
                    };
                  }
                )
              ]
            );
          })
          (
            removeAttrs args [
              "name"
              "options"
              "classOptions"
            ]
          )
      );
    };
in
{
  imports = [ module ];
}
