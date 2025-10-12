{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.lib.ordenada) mkHomeConfig;
in
{
  options.ordenada.features.kanshi = {
    enable = lib.mkEnableOption "the Kanshi feature";
    settings = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      description = "The list of profile settings to apply to Kanshi.";
      default = [ ];
    };
  };
  config.home-manager = mkHomeConfig config "kanshi" (user: {
    services.kanshi = {
      enable = true;
      settings = user.features.kanshi.settings;
    };
  });
}
