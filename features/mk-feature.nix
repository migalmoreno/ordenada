{
  name,
  options ? { },
  globals ? { },
  nixos ? { },
  homeManager ? { },
  darwin ? { },
  ...
}:
let
  module =
    { config, lib, ... }:
    let
      fname = if builtins.isList name then name else [ name ];
      mkClassConfig = class: module: extraModules: {
        ${class} =
          args@{ config, pkgs, ... }:
          let
            mod = if builtins.isFunction module then (module args) else module;
          in
          {
            imports = extraModules ++ lib.optionals (builtins.hasAttr "imports" mod) mod.imports;
            options = lib.recursiveUpdate (lib.optionalAttrs (builtins.hasAttr "options" mod) mod.options) {
              ordenada.features = lib.setAttrByPath fname (
                {
                  enable = lib.mkEnableOption "the ${name} feature";
                }
                // (if builtins.isFunction options then options args else options)
              );
            };
            config = lib.mkMerge [
              {
                ordenada.globals = if builtins.isFunction globals then (globals args) else globals;
              }
              (lib.mkIf (lib.attrByPath fname false config.ordenada.features).enable (
                removeAttrs mod [
                  "imports"
                  "options"
                ]
              ))
            ];
          };
      };
      homeModule = [
        (
          { config, ... }:
          {
            config = lib.mkIf config.ordenada.features.home.enable {
              home-manager.sharedModules = [
                {
                  ordenada.features = lib.setAttrByPath fname { enable = lib.mkDefault true; };
                }
              ];
            };
          }
        )
      ];
    in
    {
      config.ordenada.modules.${builtins.concatStringsSep "-" fname} =
        mkClassConfig "nixos" nixos homeModule
        // mkClassConfig "homeManager" homeManager [ ]
        // mkClassConfig "darwin" darwin homeModule;
    };
in
{
  imports = [ module ];
}
