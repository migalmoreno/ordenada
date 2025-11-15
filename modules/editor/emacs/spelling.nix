{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "spelling"
  ];
  options =
    { config, pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
    in
    {
      package = mkPackageOption pkgs "aspell" { };
      ispellProgram = mkOption {
        type = types.pathInStore;
        description = "Program to be used by ispell.";
        default =
          with config.ordenada.features.emacs.spelling;
          "${package}/bin/${builtins.elemAt (lib.splitString "-" (lib.getName package)) 0}";
      };
      ispellStandardDictionary = mkOption {
        type = types.str;
        description = "Default dictionary for ispell.";
        default = "";
        example = "en_US";
      };
      ispellPersonalDictionary = mkOption {
        type = types.str;
        description = "Personal dictionary for ispell.";
        default = "";
        example = "en_US";
      };
      flyspellHooks = mkOption {
        type = types.listOf types.str;
        description = "List of mode hooks where flyspell-mode should be enabled.";
        default = [ ];
        example = [
          "org-mode-hook"
          "message-mode-hook"
          "bibtex-mode-hook"
        ];
      };
      flyspellProgHooks = mkOption {
        type = types.listOf types.str;
        description = "List of mode hooks where flyspell-prog-mode should be enabled.";
        default = [ ];
        example = [ "prog-mode-hook" ];
      };
      dictionaryServer = mkOption {
        type = types.str;
        description = "Dictionary server to use.";
        default = "dict.org";
      };
      dictionaryKey = mkOption {
        type = types.str;
        description = "Keybinding used to launch the dictionary search.";
        default = "w";
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = [ config.ordenada.features.emacs.spelling.package ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-spelling";
        config =
          with config.ordenada.features.emacs.spelling; # elisp
          ''
            (mapcar (lambda (hook)
                      (add-hook hook 'flyspell-mode))
                    '(${toString flyspellHooks}))
            (mapcar (lambda (hook)
                      (add-hook hook 'flyspell-prog-mode))
                    '(${toString flyspellProgHooks}))
            (with-eval-after-load 'ispell
              (setopt ispell-program-name "${ispellProgram}")
              ${lib.optionalString (ispellStandardDictionary != "") ''
                (setopt ispell-dictionary "${ispellStandardDictionary}")
              ''}
              ${
                lib.optionalString (ispellPersonalDictionary != "") ''
                  (setopt ispell-personal-dictionary "${ispellPersonalDictionary}")
                ''
              })
            (with-eval-after-load 'flyspell
              (setopt flyspell-issue-welcome-flag nil)
              (setopt flyspell-issue-message-flag nil))
            (with-eval-after-load 'dictionary
              (setopt dictionary-server "${dictionaryServer}"))
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${dictionaryKey}" #'dictionary-search))
          '';
      };
    };
}
