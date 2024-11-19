{ lib, ... }:

rec {
  hasFeature =
    path: user:
    let
      fname = lib.splitString "." path;
    in
    lib.hasAttrByPath fname user.features && (lib.attrByPath fname false user.features).enable;

  mkHomeConfig =
    config: feature: homeConfigFn:
    let
      fname = lib.splitString "." feature;
    in
    lib.mkMerge (
      (lib.mapAttrsToList (
        name: user:
        if hasFeature feature user then
          {
            users.${name} = (
              homeConfigFn (
                lib.recursiveUpdate {
                  features = lib.setAttrByPath fname (lib.getAttrFromPath fname config.ordenada.features);
                } user
              )
            );
          }
        else
          { }
      ) config.ordenada.users)
    );
}
