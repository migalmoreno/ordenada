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
    ordenada.features.wlogout = {
      enable = mkOption {
        type = types.bool;
        default = config.ordenada.features.sway.enable;
        description = "Whether to enable the wlogout feature.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.wlogout;
        description = "The wlogout package.";
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ]; })
    {
      home-manager = mkHomeConfig config "wlogout" (user: {
        programs.wlogout = with user.features.wlogout; {
          enable = true;
          inherit package;
          style =
            with user.features.theme.scheme.withHashtag;
            let
              icons = pkgs.runCommand "wlogout-fill-icons" { } ''
                mkdir -p $out
                find ${package}/share/wlogout/assets -type f -name '*.svg' \
                  -exec sh -c 'sed "s/<svg/<svg fill=\"${base05}\"/" $0 > $out/$(basename $0)' {} \;
              '';
              mkIcons = toString (
                map (icon: ''
                  #${icon} {
                    background-image: url("${icons}/${icon}.svg");
                  }
                '') (map (icon: lib.removeSuffix ".svg" icon) (builtins.attrNames (builtins.readDir icons)))
              );
            in
            ''
              ${builtins.readFile "${pkgs.wlogout}/etc/wlogout/style.css"}

              window {
                background-color: ${
                  with pkgs.lib.nix-rice.color.hexToRgba base00;
                  "rgba(${toString r}, ${toString g}, ${toString b}, 0.9)"
                };
              }

              button {
                background-color: ${base01};
                color: ${base05};
                text-decoration-color: ${base05};
              }

              button:focus, button:active, button:hover {
                background-color: ${base02};
                color: ${base05};
              }

              ${mkIcons}
            '';
        };
      });
    }
  ];
}
