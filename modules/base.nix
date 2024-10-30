{ config, lib, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.ordenada.features;
  user = types.submodule {
    options = rec {
      name = mkOption {
        type = types.attrs;
        description = "The username for this user.";
      };
      features = mkOption {
        type = types.attrs;
        description = "Attrs of Ordenada features for this user.";
        default = config.ordenada.features;
      };
      homeDirectory = mkOption {
        type = types.str;
        description = "Home directory of this user.";
        default = "/home/${name}";
      };
    };
  };
  userInfo = types.submodule {
    options = {
      username = mkOption {
        type = types.str;
        description = "Username of Ordenada user.";
      };
      fullName = mkOption {
        type = types.str;
        description = "Fullname of Ordenada user.";
      };
      email = mkOption {
        type = types.str;
        description = "Email of Ordenada user.";
      };
      homeDirectory = mkOption {
        type = types.str;
        description = "Home directory of primary Ordenada user.";
        default = config.home-manager.users.${cfg.userInfo.username}.home.homeDirectory;
      };
    };
  };
in
{
  options = {
    ordenada.users = mkOption {
      type = types.attrsOf user;
      description = "Attrs of Ordenada users.";
    };
    ordenada.features.userInfo = mkOption {
      type = userInfo;
      description = "User information for Ordenada.";
    };
  };
}
