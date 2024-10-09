{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.xdg = {
      enable = lib.mkEnableOption "the XDG feature";
      baseDirs = lib.mkOption {
        type = lib.types.attrs;
        description = "The XDG base directories";
        default = {
          configHome = "$HOME/.config";
          dataHome = "$HOME/.local/share";
          cacheHome = "$HOME/.cache";
          stateHome = "$HOME/.local/state";
        };
      };
      userDirs = lib.mkOption {
        type = lib.types.attrs;
        description = "The XDG user directories.";
        default = {
          desktop = null;
          documents = "$HOME/documents";
          download = "$HOME/downloads";
          music = "$HOME/music";
          pictures = "$HOME/pictures";
          publicShare = "$HOME/public";
          templates = null;
          videos = "$HOME/videos";
        };
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "xdg" (user: {
      home.packages = with pkgs; [ xdg-utils ];
      xdg =
        with user.features.xdg;
        lib.mkMerge [
          {
            enable = true;
            mime.enable = true;
            mimeApps.enable = true;
            portal = {
              enable = true;
              extraPortals = [
                pkgs.xdg-desktop-portal-gtk
                pkgs.xdg-desktop-portal-wlr
              ];
              config = {
                common.default = [
                  "gtk"
                  "wlr"
                ];
              };
            };
            cacheHome = lib.mkOptionDefault baseDirs.cacheHome;
            configHome = lib.mkOptionDefault baseDirs.configHome;
            dataHome = lib.mkOptionDefault baseDirs.dataHome;
            stateHome = lib.mkOptionDefault baseDirs.stateHome;
            userDirs = userDirs;
          }
          {
            userDirs = {
              enable = true;
              createDirectories = true;
            };
          }
        ];
    });
  };
}
