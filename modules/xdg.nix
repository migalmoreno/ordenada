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
  nixos =
    { config, pkgs, ... }:
    {
      xdg.portal = with config.ordenada.globals; {
        enable = true;
        config.common.default = if (wayland == true) then "wlr" else "gtk";
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
        wlr.enable = wayland;
      };
    };
  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    with config.ordenada.globals;
    lib.mkMerge [
      (with config.ordenada.features.xdg; {
        xdg = {
          enable = true;
          cacheHome = lib.mkForce baseDirs.cacheHome;
          configHome = lib.mkForce baseDirs.configHome;
          dataHome = lib.mkForce baseDirs.dataHome;
          stateHome = lib.mkForce baseDirs.stateHome;
          userDirs = userDirs;
        };
      })
      (lib.mkIf (platform == "linux") {
        home.packages = with pkgs; [ xdg-utils ];
        xdg = {
          mime.enable = true;
          mimeApps.enable = true;
          portal = {
            enable = true;
            extraPortals =
              with pkgs;
              [
                xdg-desktop-portal-gtk
              ]
              ++ lib.optional (wayland == true) xdg.desktop-portal-wlr;
            config = {
              common.default = [
                "gtk"
              ]
              ++ lib.optonal (wayland == true) "wlr";
            };
          };
          userDirs = {
            enable = true;
            createDirectories = true;
          };
        };
      })
    ];
}
