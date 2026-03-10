{ lib, mkFeature, ... }:

let
  inherit (lib)
    types
    mkIf
    mkForce
    mkOption
    mkEnableOption
    mkPackageOption
    ;
in
mkFeature {
  name = "alacritty";
  options =
    { config, pkgs, ... }:
    let
      enabled = config.ordenada.features.alacritty.enable;
    in
    {
      package = mkPackageOption pkgs "alacritty" { };
      defaultTerminal = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global terminal.";
        default = enabled;
      };
    };
  globals =
    { config, ... }:
    {
      apps.terminal =
        with config.ordenada.features.alacritty;
        mkIf defaultTerminal (mkForce "${package}/bin/alacritty");
    };
  homeManager =
    { config, ... }:
    {
      programs.alacritty = {
        enable = true;
        settings = {
          window = {
            padding = {
              x = 8;
              y = 8;
            };
          };
          font = with config.ordenada.features.fontutils.fonts; {
            normal = {
              family = monospace.name;
              style = "Regular";
            };
            bold = {
              family = monospace.name;
              style = "Bold";
            };
            italic = {
              family = monospace.name;
              style = "Italic";
            };
            bold_italic = {
              family = monospace.name;
              style = "Bold Italic";
            };
            size = monospace.size;
          };
          colors = with config.ordenada.features.theme.scheme.withHashtag; {
            primary = {
              background = base00;
              foreground = base05;
            };
            normal = {
              black = base05;
              white = base00;
              red = base08;
              green = base0B;
              yellow = base09;
              cyan = base0C;
              magenta = base0E;
              blue = base0D;
            };
          };
        };
      };
    };
}
