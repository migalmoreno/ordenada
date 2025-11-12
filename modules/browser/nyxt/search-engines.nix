{
  inputs,
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "search-engines"
  ];
  options = {
    defaultSearchEngine = lib.mkOption {
      type = lib.types.str;
      description = "Default search engine name.";
      default = "google";
    };
    extraEngines = lib.mkOption {
      type = lib.types.attrs;
      description = "Set of extra Nyxt search engines.";
      default = { };
    };
    nxSearchEngines = lib.mkOption {
      type = lib.types.attrs;
      description = "Set of nx-search-engines engines.";
      default = {
        wordnet = {
          shortcut = "wn";
          show-examples = true;
          show-word-frequencies = true;
          show-sense-numbers = true;
        };
        github.shortcut = "gh";
        startpage.shortcut = "sp";
        sourcehut.shortcut = "sh";
        libgen = {
          shortcut = "lg";
          covers = true;
          results = 100;
          base-search-url = "https://libgen.gs/index.php?req=~a";
        };
        lemmy.shortcut = "le";
        discourse.shortcut = "ae";
        meetup.shortcut = "me";
        gitea.shortcut = "gi";
        gitea-users.shortcut = "giu";
        hacker-news.shortcut = "hn";
        lobsters.shortcut = "lo";
        reddit.shortcut = "re";
        google = {
          shortcut = "go";
          safe-search = false;
          results-number = 50;
          new-window = true;
        };
      };
    };
    autoComplete = lib.mkEnableOption "search auto complete";
    autoCompleteNonPrefix = lib.mkEnableOption "search auto complete on searches without prefixes";
  };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.dataFile."nyxt/extensions/nx-search-engines".source = inputs.nx-search-engines;
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-search-engines";
        config =
          with config.ordenada.features.nyxt.search-engines;
          let
            mkNxSearchEngines = engines: ''
              ${toString (
                lib.mapAttrsToList (name: config: ''
                  (engines:${name} ${
                    toString (
                      lib.mapAttrsToList (
                        n: v:
                        ":${n} ${
                          if builtins.isString v then
                            ''"${v}"''
                          else if builtins.isBool v then
                            ordenada-lib.lisp.toBoolean v
                          else
                            toString v
                        }"
                      ) config
                    )
                  })
                '') engines
              )}
            '';
            mkSearchEngines = engines: ''
              ${toString (
                lib.mapAttrsToList (name: config: ''
                  (make-instance 'search-engine
                                 :name "${name}"
                                 :shortcut "${config.shortcut}"
                                 :search-url "${config.search-url}"
                                 :fallback-url "${config.fallback-url}")
                '') engines
              )}
            '';
          in
          # lisp
          ''
            (define-configuration context-buffer
              ((search-auto-complete-p ${ordenada-lib.lisp.toBoolean autoComplete})
               (search-always-auto-complete-p ${ordenada-lib.lisp.toBoolean autoCompleteNonPrefix})
               (search-engines
                (append %slot-value%
                        (list ${mkSearchEngines (removeAttrs extraEngines [ defaultSearchEngine ])}
                              ${mkNxSearchEngines (removeAttrs nxSearchEngines [ defaultSearchEngine ])}
                              ${
                                if (builtins.hasAttr defaultSearchEngine extraEngines) then
                                  mkSearchEngines { ${defaultSearchEngine} = extraEngines.${defaultSearchEngine}; }
                                else if (builtins.hasAttr defaultSearchEngine nxSearchEngines) then
                                  mkNxSearchEngines { ${defaultSearchEngine} = nxSearchEngines.${defaultSearchEngine}; }
                                else
                                  ""
                              })))))
          '';
        lispPackages = [ "nx-search-engines" ];
      };
    };
}
