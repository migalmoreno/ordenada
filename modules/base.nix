{ lib, mkFeature, ... }:

mkFeature {
  name = "base";
  options =
    let
      inherit (lib) mkOption types;
    in
    {
      homePackages = mkOption {
        type = types.listOf types.package;
        description = "Provides the base Home Manager packages";
        default = [ ];
      };
      nixosPackages = mkOption {
        type = types.listOf types.package;
        description = "Provides the base NixOS packages";
        default = [ ];
      };
      darwinPackages = mkOption {
        type = types.listOf types.package;
        description = "Provides the base Darwin packages";
        default = [ ];
      };
    };
  homeManager =
    { config, ... }:
    {
      home.packages = config.ordenada.features.base.homePackages;
    };
  nixos =
    { config, ... }:
    {
      environment.systemPackages = config.ordenada.features.base.nixosPackages;
    };
  darwin =
    { config, ... }:
    {
      environment.systemPackages = config.ordenada.features.base.darwinPackages;
    };
}
