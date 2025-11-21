{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "xdg";
  options =
    { config, ... }:
    let
      inherit (lib) mkOption types;
      mkMaybeCapitalizedDir =
        path:
        let
          pred =
            config.ordenada.features.xdg.capitalizeUserDirs
            && builtins.isString path
            && lib.strings.hasInfix "/" path;
          result =
            let
              components = lib.splitString "/" path;
              dirname = lib.lists.init components;
              basename = lib.lists.last components;
            in
            "${lib.concatStringsSep "/" dirname}/${ordenada-lib.str.capitalize basename}";
        in
        if (pred) then result else path;
    in
    {
      baseDirs =
        with config.ordenada.features.userInfo;
        mkOption {
          type = ordenada-lib.types.fnOrAttrs;
          description = "The XDG base directories.";
          apply = x: ordenada-lib.getFnOrAttrsValue x ({ homeDirectory = homeDirectory; });
          default = {
            configHome = "${homeDirectory}/.config";
            dataHome = "${homeDirectory}/.local/share";
            cacheHome = "${homeDirectory}/.cache";
            stateHome = "${homeDirectory}/.local/state";
          };
        };
      userDirs =
        with config.ordenada.features.userInfo;
        mkOption {
          type = ordenada-lib.types.fnOrAttrs;
          description = "The XDG user directories.";
          apply = x: ordenada-lib.getFnOrAttrsValue x ({ homeDirectory = homeDirectory; });
          default = {
            desktop = null;
            documents = mkMaybeCapitalizedDir "${homeDirectory}/documents";
            download = mkMaybeCapitalizedDir "${homeDirectory}/downloads";
            music = mkMaybeCapitalizedDir "${homeDirectory}/music";
            pictures = mkMaybeCapitalizedDir "${homeDirectory}/pictures";
            publicShare = mkMaybeCapitalizedDir "${homeDirectory}/public";
            templates = null;
            videos = mkMaybeCapitalizedDir "${homeDirectory}/videos";
          };
        };
      capitalizeUserDirs = mkOption {
        type = types.bool;
        description = "Whether to capitalize the `userDirs` directory names.";
        default = config.ordenada.globals.platform == "darwin";
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
    with config.ordenada.features.xdg;
    lib.mkMerge [
      {
        xdg = {
          enable = true;
          cacheHome = lib.mkForce baseDirs.cacheHome;
          configHome = lib.mkForce baseDirs.configHome;
          dataHome = lib.mkForce baseDirs.dataHome;
          stateHome = lib.mkForce baseDirs.stateHome;
        };
      }
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
      (
        ## TODO: Create directories if they don't exist
        with config.ordenada.features.xdg.userDirs;
        lib.mkIf (platform == "darwin") {
          home.sessionVariables = (
            lib.attrsets.filterAttrs (_: v: v != null) {
              XDG_DESKTOP_DIR = desktop;
              XDG_DOCUMENTS_DIR = documents;
              XDG_DOWNLOAD_DIR = download;
              XDG_MUSIC_DIR = music;
              XDG_PICTURES_DIR = pictures;
              XDG_PUBLICSHARE_DIR = publicShare;
              XDG_TEMPLATES_DIR = templates;
              XDG_VIDEOS_DIR = videos;
            }
          );
        }
      )
    ];
}
