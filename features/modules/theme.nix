{
  mkFeature,
  ordenada-lib,
  lib,
  ...
}:

let
  inherit (lib) mkOption types;
  inherit (ordenada-lib.base16) mkSchemeAttrs;
in
mkFeature {
  name = "theme";
  options =
    { config, pkgs, ... }:
    let
      defaultThemeWallpapers = {
        light = (
          pkgs.fetchurl {
            url = "https://w.wallhaven.cc/full/28/wallhaven-28vjgm.jpg";
            sha256 = "14b5h86jjimdzfw9krbc90abcd9kgvfhavqqq7xzxjxjbakrkzdl";
          }
        );
        dark = (
          pkgs.fetchurl {
            url = "https://w.wallhaven.cc/full/dg/wallhaven-dgo6pl.jpg";
            sha256 = "09jap8g5232h8ham41jljvm1x7d87wjn0p42dy0x119cqd1ds1i3";
          }
        );
      };
      defaultThemeSchemes = {
        light = mkSchemeAttrs {
          base00 = "ffffff";
          base01 = "f0f0f0";
          base02 = "e0e0e0";
          base03 = "c2c2c2";
          base04 = "c4c4c4";
          base05 = "000000";
          base06 = "595959";
          base07 = "9f9f9f";
          base08 = "a60000";
          base09 = "f5d0a0";
          base0A = "6f5500";
          base0B = "00663f";
          base0C = "005e8b";
          base0D = "3548cf";
          base0E = "e07fff";
          base0F = "624416";
        };
        dark = mkSchemeAttrs {
          base00 = "000000";
          base01 = "1e1e1e";
          base02 = "313131";
          base03 = "303030";
          base04 = "646464";
          base05 = "ffffff";
          base06 = "e0e0e0";
          base07 = "0000c0";
          base08 = "ff5f59";
          base09 = "ff6b55";
          base0A = "d0bc00";
          base0B = "6ae4b9";
          base0C = "00d3d0";
          base0D = "79a8ff";
          base0E = "b6a0ff";
          base0F = "7a6100";
        };
      };
    in
    {
      polarity = mkOption {
        type = types.enum [
          "dark"
          "light"
        ];
        description = "The theme polarity.";
        default = "light";
      };
      scheme = mkOption {
        type = types.attrs;
        description = "The theme color scheme.";
        default =
          with defaultThemeSchemes;
          if config.ordenada.features.theme.polarity == "light" then light else dark;
      };
      defaultWallpapers = lib.mkOption {
        type = lib.types.attrs;
        description = "The default wallpapers.";
        default = defaultThemeWallpapers;
      };
      defaultSchemes = mkOption {
        type = types.attrs;
        description = "The default colorschemes";
        default = defaultThemeSchemes;
      };
      wallpaper = lib.mkOption {
        type = lib.types.pathInStore;
        description = "The theme wallpaper.";
        default =
          with defaultThemeWallpapers;
          if config.ordenada.features.theme.polarity == "light" then light else dark;
      };
    };
  nixos =
    { config, lib, ... }:
    let
      themeToToggle = if config.ordenada.features.theme.polarity == "dark" then "light" else "dark";
    in
    {
      security.sudo.extraRules = [
        {
          runAs = "root";
          groups = [ "wheel" ];
          commands = [
            {
              command = "/nix/var/nix/profiles/system/specialisation/${themeToToggle}/bin/switch-to-configuration switch";
              options = [ "NOPASSWD" ];
            }
            {
              command = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch";
              options = [ "NOPASSWD" ];
            }
          ];
        }
      ];
      specialisation.${themeToToggle}.configuration = {
        ordenada.features.theme = with config.ordenada.features.theme; {
          scheme = lib.mkForce defaultThemeSchemes.${themeToToggle};
          wallpaper = lib.mkForce defaultThemeWallpapers.${themeToToggle};
          polarity = lib.mkForce themeToToggle;
        };
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      themeToToggle = if config.ordenada.features.theme.polarity == "dark" then "light" else "dark";
      systemctl = "XDG_RUNTIME_DIR=\${XDG_RUNTIME_DIR:-/run/user/$UID} systemctl";
      themeToggler = pkgs.writeShellScriptBin "toggle-theme" (
        with config.ordenada.features;
        ''
          current_system=$(readlink /run/current-system)
          specialisation=$(readlink /nix/var/nix/profiles/system/specialisation/${themeToToggle})
          if [ "$current_system" == "$specialisation"] || [ ! "$specialisation" ]; then
            sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
          else
            sudo /nix/var/nix/profiles/system/specialisation/${themeToToggle}/bin/switch-to-configuration switch
          fi
          ${lib.optionalString emacs.enable ''
            ${emacs.package}/bin/emacsclient -e "(load-theme '${emacs.defaultThemes.${themeToToggle}} t)"
          ''}
          ${lib.optionalString swaync.enable ''
            ${systemctl} --user restart swaync
          ''}
        ''
      );
    in
    {
      home.packages = [ themeToggler ];
      xdg.desktopEntries.themeToggler = {
        name = "Toggle Theme";
        exec = "${themeToggler}/bin/toggle-theme %U";
      };
    };
}
