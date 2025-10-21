{
  lib,
  mkFeature,
  ...
}:

let
  inherit (lib) mkPackageOption;
in
mkFeature {
  name = "swaync";
  options =
    { pkgs, ... }:
    {
      package = mkPackageOption pkgs "swaynotificationcenter" { };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      services.swaync = {
        enable = true;
        package = config.ordenada.features.swaync.package;
        settings = {
          widgets = [
            "inhibitors"
            "title"
            "dnd"
            "mpris"
            "notifications"
          ];
        };
        # style = with config.ordenada.features.theme.scheme.withHashtag; ''
        #   @define-color noti-bg ${base01};
        #   @define-color cc-bg ${base01};
        #   @define-color noti-border-color ${base02};
        #   @define-color text-color ${base05};
        #   @define-color text-color-disabled ${base06};

        #   .control-center {
        #     font-family: ${config.ordenada.features.fontutils.fonts.sans.name};
        #     border-radius: 0;
        #     padding: 10px;
        #   }

        #   .notification {
        #     font-family: ${config.ordenada.features.fontutils.fonts.sans.name};
        #   }

        #   .notification-content {
        #     padding: 10px;
        #   }
        # '';
      };
    };
}
