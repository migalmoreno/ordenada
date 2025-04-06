{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkPackageOption;
  inherit (pkgs.lib.ordenada) mkHomeConfig;
in
{
  options.ordenada.features.playerctl = {
    enable = mkEnableOption "the playerctl feature";
    package = mkPackageOption pkgs "playerctl" { };
  };
  config.home-manager = mkHomeConfig config "playerctl" (user: {
    services.playerctld = {
      inherit (user.features.playerctl) package;
      enable = true;
    };
  });
}
