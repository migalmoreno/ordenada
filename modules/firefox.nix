{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.firefox;
  nurPkgs = import pkgs.inputs.nur {
    inherit pkgs;
    nurpkgs = pkgs;
  };
  inherit (lib) mkOption mkEnableOption types;
in
{
  options = {
    ordenada.features.firefox = {
      enable = mkEnableOption "the Firefox feature";
      package = mkOption {
        type = types.package;
        description = "The Firefox package to use.";
        default = pkgs.firefox-wayland;
      };
      extraSearchConfig = mkOption {
        type = types.attrs;
        description = "Extra search engines configuration.";
        default = { };
      };
      extraPolicies = mkOption {
        type = types.attrs;
        description = "Attrset of extra Firefox policies.";
        default = { };
      };
      extraSettings = mkOption {
        type = types.attrs;
        description = "User.js extra settings for the default Firefox profile.";
        default = { };
      };
      extraAddons = mkOption {
        type = types.listOf types.package;
        description = "Extra Firefox add-ons to install.";
        default = [ ];
      };
      arkenfoxSettings = mkOption {
        type = types.attrs;
        description = "Arkenfox user.js settings.";
        default = {
          "0000".enable = true;
          "0100".enable = true;
          "0300".enable = true;
          "0800".enable = true;
          "1700".enable = true;
          "2600".enable = true;
          "5000".enable = true;
        };
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable { environment.sessionVariables.MOZ_ENABLE_WAYLAND = 1; })
    {
      home-manager = mkHomeConfig config "firefox" (user: {
        imports = [ pkgs.inputs.arkenfox-nixos.hmModules.default ];
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
          } // extraPolicies;
          arkenfox = {
            enable = true;
            version = "128.0";
          };
          profiles = {
            default = {
              id = 0;
              name = "default";
              isDefault = true;
              arkenfox = {
                enable = true;
              } // arkenfoxSettings;
              settings = {
                "browser.aboutwelcome.enabled" = false;
                "extensions.pocket.enabled" = false;
                "trailhead.firstrun.branches" = "nofirstrun-empty";
                "browser.shell.checkDefaultBrowser" = false;
              } // extraSettings;
              search = lib.recursiveUpdate {
                force = true;
                default = "Google";
                privateDefault = "Google";
                engines = {
                  "Bing".metaData.hidden = true;
                  "Amazon.com".metaData.hidden = true;
                };
              } extraSearchConfig;
              extensions =
                with nurPkgs.repos.rycee.firefox-addons;
                [
                  ublock-origin
                  multi-account-containers
                ]
                ++ extraAddons;
            };
          };
        };
      });
    }
  ];
}
