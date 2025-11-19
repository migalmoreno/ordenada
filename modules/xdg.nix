{ lib, mkFeature, ... }:

mkFeature {
  name = "xdg";
  options =
    { config, ... }:
    let
      inherit (lib) mkOption types;
      maybeCapitalize =
        path:
        let
          capitalize =
            s:
            if lib.stringLength s < 1 then
              s
            else
              (lib.toUpper (lib.substring 0 1 s)) + (lib.substring 1 (lib.stringLength s - 1) s);
          components = lib.splitString "/" path;
          lastNonEmptyIndex =
            let
              findLastIndex =
                idx:
                if idx < 0 then
                  -1
                else if lib.elemAt components idx != "" then
                  idx
                else
                  findLastIndex (idx - 1);
            in
            findLastIndex (lib.length components - 1);
          modifiedComponents = lib.genList (
            i:
            let
              original = lib.elemAt components i;
            in
            if i == lastNonEmptyIndex then capitalize original else original
          ) (lib.length components);
          modifiedPath = lib.concatStringsSep "/" modifiedComponents;
        in
        if (config.ordenada.globals.platform == "darwin") then modifiedPath else path;
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
          documents = maybeCapitalize "${homeDirectory}/documents";
          download = maybeCapitalize "${homeDirectory}/downloads";
          music = maybeCapitalize "${homeDirectory}/music";
          pictures = maybeCapitalize "${homeDirectory}/pictures";
          publicShare = maybeCapitalize "${homeDirectory}/public";
          templates = null;
          videos = maybeCapitalize "${homeDirectory}/videos";
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
        };
      })
      (lib.mkIf (platform == "nixos") {
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
              ++ lib.optional (wayland == true) "wlr";
            };
          };
          userDirs =
            with config.ordenada.features.xdg;
            lib.mkMerge [
              userDirs
              {
                enable = true;
                createDirectories = true;
              }
            ];
        };
      })
      (with config.ordenada.features.xdg.userDirs; lib.mkIf (platform == "darwin") {
        home.sessionVariables = {
          #XDG_DESKTOP_DIR = lib.mkIf (desktop != null) desktop;
          XDG_DOCUMENTS_DIR = lib.mkIf (documents != null) documents;
          XDG_DOWNLOAD_DIR = lib.mkIf (download != null) download;
          XDG_MUSIC_DIR = lib.mkIf (music != null) music;
          XDG_PICTURES_DIR = lib.mkIf (pictures != null) pictures;
          XDG_PUBLICSHARE_DIR = lib.mkIf (publicShare != null) publicShare;
          #XDG_TEMPLATES_DIR = lib.mkIf (templates != null) templates;
          XDG_VIDEOS_DIR = lib.mkIf (videos != null) videos;
        };
      })
    ];
}
