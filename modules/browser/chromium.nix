{ lib, mkFeature, ... }:

mkFeature {
  name = "chromium";
  options =
    { pkgs, ... }:
    let
      inherit (lib)
        mkEnableOption
        mkOption
        mkPackageOption
        types
        ;
    in
    {
      package = mkPackageOption pkgs "chromium" { };
      defaultBrowser = mkEnableOption "using Chromium as your default browser";
      startupFlags = mkOption {
        type = types.listOf types.str;
        description = "Command line arguments used to launch Chromium.";
        default = [ ];
      };
      extraExtensions = mkOption {
        type = types.listOf types.attrs;
        description = ''
          List of extra Chromium extensions to install.

          To find the extension ID, check its URL on the Chrome Web Store at
          https://chrome.google.com/webstore/category/extensions

          To install extensions outside of the Chrome Web Store set updateUrl or
          crxPath and version as per the documentation at
          https://developer.chrome.com/docs/extensions/mv2/external_extensions
        '';
        default = [ ];
        example = [
          {
            id = "dcpihecpambacapedldabdbpakmachpb";
            updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
          }
          {
            id = "aaaaaaaaaabbbbbbbbbbcccccccccc";
            crxPath = "/home/share/extension.crx";
            version = "1.0";
          }
        ];
      };
    };
  homeManager =
    { config, ... }:
    lib.mkMerge [
      (lib.mkIf config.ordenada.features.chromium.defaultBrowser {
        home.sessionVariables = {
          "BROWSER" = lib.getExe config.ordenada.features.chromium.package;
        };
        xdg.mimeApps.defaultApplications = lib.genAttrs [
          "text/html"
          "text/xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/about"
        ] (lib.const "chromium.desktop");
      })
      {
        programs.chromium = with config.ordenada.features.chromium; {
          inherit package;
          enable = true;
          commandLineArgs = startupFlags;
          extensions = [
            { id = "ddkjiahejlhfcafbddmgiahcphecmpfh"; } # ublock origin lite
          ]
          ++ extraExtensions;
        };
      }
    ];
}
