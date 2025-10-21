{ lib }:

builtins.concatLists (
  map (
    file:
    let
      module = import file;
      useMkFeature = builtins.isFunction module && (builtins.functionArgs module) ? mkFeature;
      mkFeature = import ./mk-feature.nix;
    in
    lib.optional (builtins.pathExists file) (
      if useMkFeature then
        args@{ config, ... }:
        let
          context = name: ''while evaluating the module argument `${name}' in "${toString file}":'';
          extraArgs = lib.pipe module [
            builtins.functionArgs
            (lib.flip removeAttrs [ "mkFeature" ])
            (builtins.mapAttrs (
              name: _: builtins.addErrorContext (context name) (args.${name} or config._module.args.${name})
            ))
          ];
        in
        {
          key = file;
          _file = file;
          imports = [
            (module (args // extraArgs // { inherit mkFeature; }))
          ];
        }
      else
        file
    )
  ) (lib.filesystem.listFilesRecursive ./modules)
)
