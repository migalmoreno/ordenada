{ config, lib, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.ordenada.features;
  userModule =
    { name, config, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          description = "The username for this user.";
          default = name;
        };
        features = mkOption {
          type = types.attrs;
          description = "Attrs of Ordenada features for this user.";
          default = cfg;
        };
      };
    };
  userInfo = types.submodule {
    options = {
      username = mkOption {
        type = types.str;
        description = "Username of Ordenada user.";
        default = "nixos";
      };
      fullName = mkOption {
        type = types.str;
        description = "Fullname of Ordenada user.";
        default = "";
      };
      email = mkOption {
        type = types.str;
        description = "Email of Ordenada user.";
        default = "";
      };
      homeDirectory = mkOption {
        type = types.str;
        description = "Home directory of primary Ordenada user.";
        default = "/home/${cfg.userInfo.username}";
      };
      gpgPrimaryKey = mkOption {
        type = types.nullOr types.str;
        description = "The primary GnuPG key for this user.";
        default = null;
      };
    };
  };
in
{
  options.ordenada = {
    users = mkOption {
      type = types.attrsOf (types.submodule userModule);
      description = "Attrs of Ordenada users.";
      default = { };
    };
    features.userInfo = mkOption {
      type = userInfo;
      description = "User information for Ordenada.";
      default = { };
    };
  };
}
