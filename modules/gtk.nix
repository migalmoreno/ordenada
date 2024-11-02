{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features;
  themeModule = lib.types.submodule {
    options = {
      name = lib.mkOption {
        description = "Name of the theme.";
        type = lib.types.str;
      };
      package = lib.mkOption {
        description = "Theme package.";
        type = lib.types.package;
      };
    };
  };
in
{
  options = {
    ordenada.features.gtk = {
      enable = lib.mkEnableOption "the GTK feature";
      defaultThemes = lib.mkOption {
        type = lib.types.attrsOf themeModule;
        description = "The default GTK themes.";
        default = {
          light = {
            name = "adw-gtk3";
            package = pkgs.adw-gtk3;
          };
          dark = {
            name = "adw-gtk3-dark";
            package = pkgs.adw-gtk3;
          };
        };
      };
      theme = lib.mkOption {
        type = themeModule;
        description = "The GTK theme.";
        default = config.ordenada.features.gtk.defaultThemes.${cfg.theme.polarity};
      };
      cursorTheme = lib.mkOption {
        type = themeModule;
        description = "The cursor theme.";
        default = {
          name = "Bibata-Modern-${if cfg.theme.polarity == "dark" then "Classic" else "Ice"}";
          package = pkgs.bibata-cursors;
        };
      };
      cursorSize = lib.mkOption {
        type = lib.types.int;
        description = "The cursor size.";
        default = 24;
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "gtk" (
      user: with user.features.gtk; {
        home.packages = with pkgs; [
          dconf
          adwaita-icon-theme
          gnome-tweaks
        ];
        home.pointerCursor = lib.mkDefault {
          inherit (cursorTheme) name package;
          gtk.enable = true;
        };
        gtk = {
          enable = true;
          theme = theme;
          cursorTheme = cursorTheme;
        };
      }
    );
  };
}
