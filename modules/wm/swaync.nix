{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption types;
in
{
  options = {
    ordenada.features.swaync = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.sway.enable;
        description = "Whether to enable the swaync feature.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.swaync;
        description = "The swaync package.";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "swaync" (user: {
      services.swaync = {
        enable = true;
        style = with user.features.theme.scheme.withHashtag; ''
          @define-color noti-bg ${base01};
          @define-color cc-bg ${base01};
          @define-color noti-border-color ${base02};

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
  };
}
