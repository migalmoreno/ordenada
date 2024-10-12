{ config, lib, ... }:
let
  cfg = config.ordenada.features;
  user = lib.types.submodule {
    options = rec {
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
    ordenada.users = lib.mkOption {
      type = lib.types.attrsOf user;
      description = "Attrs of Ordenada users.";
    };
    ordenada.features.userInfo = lib.mkOption {
      type = userInfo;
      description = "User information for Ordenada.";
    };
  };
}
