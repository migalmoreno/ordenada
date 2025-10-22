{ lib, mkFeature, ... }:

mkFeature {
  name = "playerctl";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "playerctl" { };
    };
  homeManager =
    { config, ... }:
    {
      services.playerctld = {
        inherit (config.ordenada.features.playerctl) package;
        enable = true;
      };
    };
}
