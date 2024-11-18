{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features;
  themeModule = types.submodule {
    options = {
      name = mkOption {
        description = "Name of the theme.";
        type = types.str;
      };
      package = mkOption {
        description = "Theme package.";
        type = types.package;
      };
    };
  };
in
{
  options = {
    ordenada.features.gtk = {
      enable = mkEnableOption "the GTK feature";
      defaultThemes = mkOption {
        type = types.attrsOf themeModule;
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
      theme = mkOption {
        type = themeModule;
        description = "The GTK theme.";
        default = cfg.gtk.defaultThemes.${cfg.theme.polarity};
      };
      iconTheme = mkOption {
        type = themeModule;
        description = "The icon theme.";
        default = {
          name = "Adwaita";
          package = pkgs.adwaita-icon-theme;
        };
      };
      cursorTheme = mkOption {
        type = themeModule;
        description = "The cursor theme.";
        default = {
          name = "Bibata-Modern-${if cfg.theme.polarity == "dark" then "Classic" else "Ice"}";
          package = pkgs.bibata-cursors;
        };
      };
      cursorSize = mkOption {
        type = types.int;
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
          gnome-tweaks
        ];
        home.pointerCursor = lib.mkDefault {
          inherit (cursorTheme) name package;
          gtk.enable = true;
        };
        gtk = {
          enable = true;
          inherit theme cursorTheme iconTheme;
        };
      }
    );
  };
}
