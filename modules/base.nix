{ config, lib, ... }:
let
  cfg = config.ordenada.features;
  userInfo = lib.types.submodule {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
        description = "Primary Ordenada username.";
      };
      homeDirectory = lib.mkOption {
        type = lib.types.str;
        description = "Home directory of primary Ordenada user.";
        default = config.home-manager.users.${cfg.userInfo.username}.home.homeDirectory;
      };
    };
  };
in
{
  options = {
    ordenada.features.userInfo = lib.mkOption {
      type = userInfo;
      description = "User information for Ordenada.";
    };
  };
}
