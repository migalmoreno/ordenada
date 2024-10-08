{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.firefox;
in
{
  options = {
    ordenada.features.firefox = {
      enable = lib.mkEnableOption "the Firefox feature";
      package = lib.mkOption {
        type = lib.types.package;
        description = "The Firefox package to use.";
        default = pkgs.firefox-wayland;
      };
      extraSearchConfig = lib.mkOption {
        type = lib.types.attrs;
        description = "Extra search engines configuration.";
        default = { };
      };
      extraSettings = lib.mkOption {
        type = lib.types.attrs;
        description = "Extra preferences.";
        default = { };
      };
      extensions = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        description = "The Firefox extensions to install.";
        default = [ ];
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { environment.sessionVariables.MOZ_ENABLE_WAYLAND = 1; })
    {
      home-manager = mkHomeConfig config "firefox" (user: {
        xdg.mimeApps.defaultApplications = {
          "text/html" = [ "firefox.desktop" ];
          "text/xml" = [ "firefox.desktop" ];
          "x-scheme-handler/http" = [ "firefox.desktop" ];
          "x-scheme-handler/https" = [ "firefox.desktop" ];
        };
        programs.firefox = with user.features.firefox; {
          enable = true;
          package = package;
          policies = {
            FirefoxHome = {
              Search = true;
              TopSites = false;
              SponsoredTopSites = false;
              Highlights = false;
              Pocket = false;
              SponsoredPocket = false;
              Snippets = false;
              Locked = true;
            };
            FirefoxSuggest = {
              WebSuggestions = false;
              SponsoredSuggestions = false;
              ImproveSuggest = false;
              Locked = true;
            };
          };
          profiles = {
            default = {
              id = 0;
              name = "default";
              isDefault = true;
              settings = {
                "accessibility.typeaheadfind.enablesound" = false;
                "browser.aboutConfig.showWarning" = false;
                "browser.aboutwelcome.enabled" = false;
                "browser.ctrlTab.sortByRecentlyUsed" = true;
                "browser.download.useDownloadDir" = false;
                "browser.formfill.enable" = false;
                "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "";
                "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "";
                "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
                "browser.sessionstore" = false;
                "browser.sessionstore.max_tabs_undo" = 0;
                "browser.sessionstore.resume_session_once" = true;
                "browser.shell.checkDefaultBrowser" = false;
                "browser.toolbars.bookmarks.visibility" = "never";
                "browser.topsites.contile.enabled" = false;
                "browser.translations.enable" = false;
                "browser.urlbar.placeholderName" = "Whoogle";
                "browser.urlbar.maxRichResults" = 0;
                "browser.urlbar.suggest.addons" = false;
                "browser.urlbar.suggest.engines" = false;
                "browser.urlbar.suggest.history" = false;
                "browser.urlbar.suggest.openpage" = false;
                "browser.urlbar.suggest.recentsearches" = false;
                "browser.urlbar.suggest.searches" = false;
                "browser.urlbar.suggest.topsites" = false;
                "extensions.autoDisableScopes" = 0;
                "extensions.enabledScopes" = 15;
                "extensions.htmlaboutaddons.recommendations.enabled" = false;
                "extensions.pocket.enabled" = false;
                "general.smoothScroll" = false;
                "general.autoScroll" = true;
                "identity.fxaccounts.enabled" = false;
                "network.protocol-handler.external.mailto" = false;
                "places.history.enabled" = false;
                "privacy.trackingprotection.enabled" = true;
                "privacy.trackingprotection.socialtracking.enabled" = true;
                "privacy.userContext.enabled" = true;
                "privacy.userContext.ui.enabled" = true;
                "signon.rememberSignons" = false;
                "trailhead.firstrun.branches" = "nofirstrun-empty";
              } // extraSettings;
              search = lib.attrsets.recursiveUpdate {
                force = true;
                default = "Google";
                privateDefault = "Google";
                engines = {
                  "Bing".metaData.hidden = true;
                  "Amazon.com".metaData.hidden = true;
                };
              } extraSearchConfig;
              extensions = extensions;
            };
          };
        };
      });
    }
  ];
}
