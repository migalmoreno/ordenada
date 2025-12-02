{
  lib,
  mkFeature,
  ...
}:

mkFeature {
  name = "homebrew";
  options =
    { config, ... }:
    let
      inherit (lib) types mkOption;
    in
    {
      repositories = mkOption {
        type = types.listOf types.str;
        description = "Additional repositories for homebrew to tap into.";
        default = [ ];
      };
      masPackages = mkOption {
        type = types.attrs;
        description = "Attribute set of apps to install from the Mac app store.";
        default = {};
        example = ''
          {
            "1Password for Safari" = 1569813296;
            Xcode = 497799835;
          }
        '';
      };
      updateOnSwitch = mkOption {
        type = types.bool;
        description = "Whether to update outdated formulae and casks when switching to a new configuration.";
        default = false;
      };
    };
  darwin =
    { config, ... }:
    let
      features = config.ordenada.features;
    in
    {
      system.primaryUser = features.userInfo.username; # NOTE: Is required for homebrew to work
      environment.systemPath = [
        "/opt/homebrew/bin"
        "/opt/homebrew/sbin"
      ];
      homebrew = {
        enable = true;
        brews = [
          "mas"
        ];
        taps = features.homebrew.repositories;
        masApps = features.homebrew.masPackages;
        onActivation.cleanup = "uninstall";
        onActivation.upgrade = features.homebrew.updateOnSwitch;
      };
    };
}
