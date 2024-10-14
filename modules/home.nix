{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.home;
in
{
  options = {
    ordenada.features.home = {
      enable = mkEnableTrueOption "the home feature";
      extraGroups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The extra list of groups.";
        default = [ ];
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.backupFileExtension = "backup";
    })
    {
      home-manager = mkHomeConfig config "home" (user: {
        programs.home-manager.enable = true;
        targets.genericLinux.enable = true;
        home.stateVersion = "24.05";
      });
      users = mkHomeConfig config "home" (user: {
        isNormalUser = true;
        extraGroups = [ "wheel" ] ++ user.features.home.extraGroups;
      });
    }
  ];
}
