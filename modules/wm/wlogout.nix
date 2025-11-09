{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "wlogout";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "wlogout" { };
    };
  nixos =
    { pkgs, ... }:
    {
      programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.wlogout = with config.ordenada.features.wlogout; {
        inherit package;
        enable = true;
        style =
          with config.ordenada.features.theme.scheme.withHashtag;
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
          # css
          ''
            ${builtins.readFile "${pkgs.wlogout}/etc/wlogout/style.css"}

            window {
              background-color: ${
                with ordenada-lib.nix-rice.color.hexToRgba base00;
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
    };
}
