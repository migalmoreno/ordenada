{ config, lib, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.ordenada.features;
  user = types.submodule {
    options = {
      name = mkOption {
        type = types.attrs;
        description = "The username for this user.";
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
    };
  };
in
{
  options = {
    ordenada.users = mkOption {
      type = types.attrsOf user;
      description = "Attrs of Ordenada users.";
      default = { };
    };
    ordenada.features.userInfo = mkOption {
      type = userInfo;
      description = "User information for Ordenada.";
      default = { };
    };
  };
}
