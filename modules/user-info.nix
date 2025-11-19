{ lib, mkFeature, ... }:

mkFeature {
  name = "userInfo";
  options =
    { config, ... }:
    let
      inherit (lib) mkOption types;
    in
    {
      username = mkOption {
        type = types.str;
        description = "Username of Ordenada user.";
        default = "user";
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
      homeDirectory =
        with config.ordenada.features.userInfo;
        mkOption {
          type = types.str;
          description = "Home directory of primary Ordenada user.";
          default =
            if (config.ordenada.globals.platform == "darwin") then "/Users/${username}" else "/home/${username}";
        };
      gpgPrimaryKey = mkOption {
        type = types.nullOr types.str;
        description = "The primary GnuPG key for this user.";
        default = null;
      };
      locale = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "The locale used for this user.";
        default = "en_US.UTF-8";
      };
      extraGroups = mkOption {
        type = types.listOf types.str;
        description = "The list of extra groups for this user.";
        default = [
          "wheel"
          "netdev"
          "audio"
          "video"
          "dialout"
        ];
      };
    };
  nixos =
    { config, ... }:
    {
      ordenada.globals.platform = "nixos";
      users.users = with config.ordenada.features.userInfo; {
        ${username} = {
          inherit extraGroups;
          isNormalUser = true;
          home = homeDirectory;
          description = fullName;
        };
      };
    };
  homeManager =
    { config, ... }:
    {
      home = with config.ordenada.features.userInfo; {
        inherit username homeDirectory;
      };
    };
}
