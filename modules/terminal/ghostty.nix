{ lib, mkFeature, ... }:

let
  inherit (lib)
    mkIf
    mkMerge
    ;
in
mkFeature {
  name = "ghostty";
  options =
    { config, pkgs, ... }:
    {
      package =
        let
          platformPackage =
            if (config.ordenada.globals.platform == "darwin") then "ghostty-bin" else "ghostty";
        in
        lib.mkPackageOption pkgs platformPackage { };
    };
  globals =
    { config, ... }:
    {
      # apps.terminal =
      #   let
      #     platformBinPath =
      #       if (config.ordenada.globals.platform == "darwin") then
      #         "/Applications/Ghostty.app/Contents/MacOS/ghostty"
      #       else
      #         "/bin/ghostty";
      #   in
      #   with config.ordenada.features.alacritty;
      #   lib.mkForce "${package}/${platformBinPath}";
    };
  homeManager =
    { config, ... }:
    {
      programs.ghostty = mkMerge [
        (mkIf (config.ordenada.globals.platform == "linux") {
          systemd.enable = true;
        })
        {
          enable = true;
          package = config.ordenada.features.ghostty.package;
          enableZshIntegration = config.ordenada.features.zsh.enable;
          enableBashIntegration = config.ordenada.features.bash.enable;

          settings = with config.ordenada.features.fontutils.fonts; {
            theme = "ordenada";

            font-family = monospace.name;
            font-size = monospace.size;
            font-style = "Medium";
            font-style-bold = "Bold";
            font-style-italic = "Italic";
            font-style-bold-italic = "Bold Italic";

            window-padding-x = 8;
            window-padding-y = 8;

            clipboard-read = "allow";
            clipboard-write = "allow";
            clipboard-paste-protection = false;
          };

          themes = with config.ordenada.features.theme.scheme.withHashtag; {
            ordenada = {
              background = base00;
              foreground = base05;
              palette = [
                "0=${base05}"
                "1=${base06}"
                "2=${base07}"
                "3=${base03}"
                "4=${base04}"
                "5=${base02}"
                "6=${base01}"
                "7=${base00}"
                "8=${base05}" # white
                "9=${base08}" # red
                "10=${base0B}" # green
                "11=${base09}" # yellow
                "12=${base0D}" # blue
                "13=${base0E}" # magenta
                "14=${base0C}" # cyan
                "15=${base00}" # black
              ];
            };
          };
        }
      ];
    };
}
