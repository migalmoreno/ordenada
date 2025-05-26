{ config, lib, pkgs, ... }:

with pkgs.lib.ordenada;

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.ordenada.features.scripts.screenshot;
  grimScript = { region ? false, clipboard ? false, }:
    pkgs.writeShellScriptBin "screenshot${if region then "-region" else ""}${
      if clipboard then "-clipboard" else ""
    }" ''
      ${pkgs.grim}/bin/grim ${
        if region then ''-g "$(${pkgs.slurp}/bin/slurp)"'' else ""
      } \
      ${if clipboard then
        "- |  ${pkgs.wl-clipboard}/bin/wl-copy"
      else
        "-t jpeg ${config.ordenada.features.xdg.userDirs.pictures}/$(date +%Y%m%d-%H%M%S)"}
    '';
  grimScriptRegionClipboard = grimScript {
    region = true;
    clipboard = true;
  };
  wfRecorderScript = { region ? false, }:
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
in {
  options = {
    ordenada.features.scripts.screenshot = {
      enable = mkEnableOption "the screenshot script module";
    };
  };

  config = {
    home-manager = lib.mkIf cfg.enable (lib.mkMerge [
      (lib.mkIf config.ordenada.globals.wayland (lib.mkMerge [
        ## TODO: Implement wizard and bind it to print
        (mkHomeConfig config "scripts.screenshot" (user: {
          xdg.desktopEntries = {
            screenshot = {
              name = "Screenshot Region";
              exec =
                "${grimScriptRegionClipboard}/bin/screenshot-region-clipboard %U";
            };
            screencast = {
              name = "Screencast Region";
              exec = "${wfRecorderScriptRegion}/bin/screencast-region %U";
            };
          };
          home.packages = with pkgs; [
            grimScriptRegionClipboard
            wf-recorder
            wfRecorderScriptRegion
            convertToGif
          ];
        }))
        {
          ## TODO: Implement x11 screenshot script once we have an x11 module
        }
      ]))
    ]);
  };
}
