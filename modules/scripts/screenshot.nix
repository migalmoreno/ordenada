{ mkFeature, ... }:

mkFeature {
  name = [
    "scripts"
    "screenshot"
  ];
  homeManager =
    { config, pkgs, ... }:
    let
      grimScript =
        {
          region ? false,
          clipboard ? false,
        }:
        pkgs.writeShellScriptBin "screenshot${if region then "-region" else ""}${if clipboard then "-clipboard" else ""}" ''
          ${pkgs.grim}/bin/grim ${if region then ''-g "$(${pkgs.slurp}/bin/slurp)"'' else ""} \
          ${
            if clipboard then
              "- |  ${pkgs.wl-clipboard}/bin/wl-copy"
            else
              "-t jpeg ${config.ordenada.features.xdg.userDirs.pictures}/$(date +%Y%m%d-%H%M%S)"
          }
        '';
      grimScriptRegionClipboard = grimScript {
        region = true;
        clipboard = true;
      };
      wfRecorderScript =
        {
          region ? false,
        }:
        pkgs.writeShellScriptBin "screencast${if region then "-region" else ""}" ''
          ${pkgs.wf-recorder}/bin/wf-recorder -x yuv420p \
          ${if region then ''-g "$(${pkgs.slurp}/bin/slurp)"'' else ""} \
          -f ${config.ordenada.features.xdg.userDirs.videos}/$(date +%Y%m%d-%H%M%S).mp4
        '';
      wfRecorderScriptRegion = wfRecorderScript { region = true; };
      convertToGif = pkgs.writeShellScriptBin "convert-to-gif" ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i "$1" \
        -filter_complex "[0:v] palettegen" /tmp/gif_palette.gif
        ${pkgs.ffmpeg}/bin/ffmpeg -i "$1" -i /tmp/gif_palette.gif \
        -filter_complex "[0:v] fps=10,scale=720:-1 [new];[new][1:v] paletteuse" "$2"
        ${pkgs.wl-clipboard}/bin/wl-copy -t image/png < "$2"
      '';
    in
    {
      xdg.desktopEntries = {
        screenshot = {
          name = "Screenshot";
          exec = "${(grimScript { })}/bin/screenshot %U";
        };
        screenshot-region = {
          name = "Screenshot Region";
          exec = "${grimScriptRegionClipboard}/bin/screenshot-region-clipboard %U";
        };
        screencast = {
          name = "Screencast";
          exec = "${(wfRecorderScript { })}/bin/screencast %U";
        };
        screencast-region = {
          name = "Screencast Region";
          exec = "${wfRecorderScriptRegion}/bin/screencast-region %U";
        };
      };
      home.packages = with pkgs; [
        (grimScript { })
        grimScriptRegionClipboard
        wf-recorder
        (wfRecorderScript { })
        wfRecorderScriptRegion
        convertToGif
      ];
    };
}
