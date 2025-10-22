{ lib, mkFeature, ... }:

mkFeature {
  name = "swaylock";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "swaylock-effects" { };
    };
  nixos.security.pam.services.swaylock = { };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.swaylock =
        let
          transparent = "00000000";
        in
        {
          enable = true;
          inherit (config.ordenada.features.swaylock) package;
          settings = with config.ordenada.features.theme.scheme; {
            clock = true;
            indicator = true;
            indicator-thickness = 7;
            effect-vignette = "0.5:0.5";
            hide-keyboard-layout = true;
            image = "${config.ordenada.features.theme.wallpaper}";
            color = base00;
            inside-color = base00;
            inside-clear-color = base00;
            inside-caps-lock-color = base00;
            inside-ver-color = base00;
            inside-wrong-color = base00;
            key-hl-color = base0B;
            layout-bg-color = base00;
            layout-border-color = base01;
            layout-text-color = base05;
            line-uses-inside = false;
            line-uses-ring = false;
            line-color = transparent;
            line-ver-color = transparent;
            line-clear-color = transparent;
            line-wrong-color = transparent;
            ring-color = base01;
            ring-clear-color = base0A;
            ring-caps-lock-color = base01;
            ring-ver-color = base0B;
            ring-wrong-color = base08;
            separator-color = transparent;
            text-color = base05;
            text-clear-color = base05;
            text-caps-lock-color = base05;
            text-ver-color = base05;
            text-wrong-color = base05;
            font = config.ordenada.features.fontutils.fonts.sans.name;
          };
        };
    };
}
