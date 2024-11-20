{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption types;
  cfg = config.ordenada.features.swaylock;
in
{
  options = {
    ordenada.features.swaylock = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.sway.enable;
        description = "Whether to enable the swaylock feature.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.swaylock;
        description = "The swaylock package.";
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { security.pam.services.swaylock = { }; })
    {
      home-manager = mkHomeConfig config "swaylock" (user: {
        programs.swaylock =
          with user.features.theme.scheme;
          let
            transparent = "00000000";
          in
          {
            enable = true;
            package = pkgs.swaylock-effects;
            settings = {
              clock = true;
              indicator = true;
              indicator-thickness = 7;
              effect-vignette = "0.5:0.5";
              hide-keyboard-layout = true;
              image = "${user.features.theme.wallpaper}";
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
              font = user.features.fontutils.fonts.sans.name;
            };
          };
      });
    }
  ];
}
