{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

let
  cfg = config.ordenada.features.emacs.spelling;
  inherit (lib) types mkOption mkEnableOption;
in
{
  options = {
    ordenada.features.emacs.spelling = {
      enable = mkEnableOption "the Emacs spelling feature";
      package = mkOption {
        type = types.package;
        description = "Package to be used for spelling.";
        default = pkgs.aspell;
      };
      ispellProgram = mkOption {
        type = types.pathInStore;
        description = "Program to be used by ispell.";
        default =
          with cfg;
          "${package}/bin/${builtins.elemAt (lib.splitString "-" (lib.getName package)) 0}";
      };
      ispellStandardDictionary = mkOption {
        type = types.str;
        description = "Default dictionary for ispell.";
        default = "";
      };
      ispellPersonalDictionary = mkOption {
        type = types.str;
        description = "Personal dictionary for ispell.";
        default = "";
      };
      flyspellHooks = mkOption {
        type = types.listOf types.str;
        description = "List of mode hooks where flyspell-mode should be enabled.";
        default = [ ];
      };
      flyspellProgHooks = mkOption {
        type = types.listOf types.str;
        description = "List of mode hooks where flyspell-prog-mode should be enabled.";
        default = [ ];
      };
      dictionaryServer = mkOption {
        type = types.str;
        description = "Dictionary server to use.";
        default = "dict.org";
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.spelling" (user: {
      home.packages = [ user.features.emacs.spelling.package ];
      programs.emacs = mkElispConfig {
        name = "ordenada-spelling";
        config = with user.features.emacs.spelling; ''
          (mapcar (lambda (hook)
                    (add-hook hook 'flyspell-mode))
                  '(${toString flyspellHooks}))
          (mapcar (lambda (hook)
                    (add-hook hook 'flyspell-prog-mode))
                  '(${toString flyspellProgHooks}))
          (with-eval-after-load 'ispell
            (setq ispell-program-name "${ispellProgram}")
            ${
              if ispellStandardDictionary != "" then
                ''
                  (setq ispell-dictionary "${ispellStandardDictionary}")
                ''
              else
                ""
            }
            ${
              if ispellPersonalDictionary != "" then
                ''
                  (setq ispell-personal-dictionary "${ispellPersonalDictionary}")
                ''
              else
                ""
            })
          (with-eval-after-load 'flyspell
            (setq flyspell-issue-welcome-flag nil)
            (setq flyspell-issue-message-flag nil))
          (with-eval-after-load 'dictionary
            (setq dictionary-server "${dictionaryServer}"))
          (with-eval-after-load 'ordenada-keymaps
            (define-key ordenada-app-map (kbd "d") 'dictionary-search))
        '';
      };
    });
  };
}
