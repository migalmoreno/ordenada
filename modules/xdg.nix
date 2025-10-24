{ lib, mkFeature, ... }:

mkFeature {
  name = "xdg";
  options =
    { config, ... }:
    let
      inherit (lib) mkOption types;
    in
    {
      baseDirs = mkOption {
        type = types.attrs;
        description = "The XDG base directories.";
        default = with config.ordenada.features.userInfo; {
          configHome = "${homeDirectory}/.config";
          dataHome = "${homeDirectory}/.local/share";
          cacheHome = "${homeDirectory}/.cache";
          stateHome = "${homeDirectory}/.local/state";
        };
      };
      userDirs = mkOption {
        type = types.attrs;
        description = "The XDG user directories.";
        default = with config.ordenada.features.userInfo; {
          desktop = null;
          documents = "${homeDirectory}/documents";
          download = "${homeDirectory}/downloads";
          music = "${homeDirectory}/music";
          pictures = "${homeDirectory}/pictures";
          publicShare = "${homeDirectory}/public";
          templates = null;
          videos = "${homeDirectory}/videos";
        };
      };
    };
  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      home.packages = with pkgs; [ xdg-utils ];
      xdg = lib.mkMerge [
        (with config.ordenada.features.xdg; {
          enable = true;
          mime.enable = true;
          mimeApps.enable = true;
          portal = {
            enable = true;
            extraPortals = with pkgs; [
              xdg-desktop-portal-gtk
              xdg-desktop-portal-wlr
            ];
            config = {
              common.default = [
                "gtk"
                "wlr"
              ];
            };
          };
          cacheHome = lib.mkForce baseDirs.cacheHome;
          configHome = lib.mkForce baseDirs.configHome;
          dataHome = lib.mkForce baseDirs.dataHome;
          stateHome = lib.mkForce baseDirs.stateHome;
          userDirs = userDirs;
        })
        {
          userDirs = {
            enable = true;
            createDirectories = true;
          };
        }
      ];
    };
}
