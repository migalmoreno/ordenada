{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "age";
  options =
    { pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
    in
    {
      package = mkPackageOption pkgs "age" { };
      identities = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The list of age identities.";
      };
      recipients = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The list of age recipients.";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = [ config.ordenada.features.age.package ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-age";
        config =
          with config.ordenada.features.age;
          with ordenada-lib.elisp;
          ''
            (require 'age)
            (age-file-enable)
            (setopt age-program "${lib.getExe package}")
            (setopt age-default-identity ${toList identities})
            (setopt age-default-recipient ${toList recipients})
          '';
        elispPackages = with pkgs.emacsPackages; [ age ];
      };
    };
}
