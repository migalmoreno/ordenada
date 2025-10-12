{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkPackageOption
    types
    ;
  inherit (pkgs.lib.ordenada) elisp mkElispConfig mkHomeConfig;
in
{
  options.ordenada.features.age = {
    enable = mkEnableOption "the age feature";
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
  config.home-manager = mkHomeConfig config "age" (user: {
    home.packages = [ user.features.age.package ];
    programs.emacs = mkElispConfig {
      name = "ordenada-age";
      config =
        with user.features.age;
        with elisp;
        ''
          (require 'age)
          (age-file-enable)
          (setopt age-program "${lib.getExe package}")
          (setopt age-default-identity ${mkList identities})
          (setopt age-default-recipient ${mkList recipients})
        '';
      elispPackages = with pkgs.emacsPackages; [ age ];
    };
  });
}
