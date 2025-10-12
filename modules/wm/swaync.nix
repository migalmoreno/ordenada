{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption mkPackageOption types;
  inherit (pkgs.lib.ordenada) mkHomeConfig;
in
{
  options.ordenada.features.swaync = {
    enable = mkOption {
      type = types.bool;
      default = config.ordenada.features.sway.enable;
      description = "Whether to enable the swaync feature.";
    };
    package = mkPackageOption pkgs "swaynotificationcenter" { };
  };
  config.home-manager = mkHomeConfig config "swaync" (user: {
    services.swaync = {
      enable = true;
      settings = {
        widgets = [
          "inhibitors"
          "title"
          "dnd"
          "mpris"
          "notifications"
        ];
      };
      style = with user.features.theme.scheme.withHashtag; ''
        @define-color noti-bg ${base01};
        @define-color cc-bg ${base01};
        @define-color noti-border-color ${base02};
        @define-color text-color ${base05};
        @define-color text-color-disabled ${base06};

        .control-center {
          font-family: ${user.features.fontutils.fonts.sans.name};
          border-radius: 0;
          padding: 10px;
        }

        .notification {
          font-family: ${user.features.fontutils.fonts.sans.name};
        }

        .notification-content {
          padding: 10px;
        }
      '';
    };
  });
}
