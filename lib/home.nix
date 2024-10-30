{ lib, ... }:

rec {
  hasFeature =
    path: user:
    let
      fname = lib.strings.splitString "." path;
    in
    lib.attrsets.hasAttrByPath fname user.features
    && (lib.attrsets.attrByPath fname false user.features).enable;

  mkHomeConfig =
    config: feature: homeConfigFn:
    let
      fname = lib.strings.splitString "." feature;
    in
    lib.mkMerge (
      (lib.attrsets.mapAttrsToList (
        name: user:
        if hasFeature feature user then
          {
            users.${name} = (
              homeConfigFn (
                lib.recursiveUpdate {
                  features = (
                    lib.attrsets.setAttrByPath fname (lib.attrsets.getAttrFromPath fname config.ordenada.features)
                  );
                } user
              )
            );
          }
        else
          { }
      ) config.ordenada.users)
    );
}
