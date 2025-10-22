{ lib, mkFeature, ... }:

mkFeature {
  name = "gtk";
  options =
    { config, pkgs, ... }:
    let
      inherit (lib) mkOption types;
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
        default = with config.ordenada.features; gtk.defaultThemes.${theme.polarity};
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
          name = "Bibata-Modern-${
            if config.ordenada.features.theme.polarity == "dark" then "Classic" else "Ice"
          }";
          package = pkgs.bibata-cursors;
        };
      };
      cursorSize = mkOption {
        type = types.int;
        description = "The cursor size.";
        default = 24;
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        dconf
        gnome-tweaks
      ];
      home.pointerCursor = lib.mkDefault {
        inherit (config.ordenada.features.gtk.cursorTheme) name package;
        gtk.enable = true;
      };
      gtk = with config.ordenada.features.gtk; {
        inherit theme cursorTheme iconTheme;
        enable = true;
      };
    };
}
