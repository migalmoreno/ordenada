{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.home;
  user = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.attrs;
        description = "The username for this user.";
      };
      features = lib.mkOption {
        type = lib.types.attrs;
        description = "Attrs of Ordenada features for this user.";
        default = config.ordenada.features;
      };
      homeDirectory = lib.mkOption {
        type = lib.types.str;
        description = "Home directory of this user.";
        default = "/home/${name}";
      };
    };
  };
in
{
  options = {
    ordenada.users = lib.mkOption {
      type = lib.types.attrsOf user;
      description = "Attrs of Ordenada users.";
    };
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
        home.stateVersion = "23.05";
      });
      users = mkHomeConfig config "home" (user: {
        isNormalUser = true;
        extraGroups = [ "wheel" ] ++ user.features.home.extraGroups;
      });
    }
  ];
}
