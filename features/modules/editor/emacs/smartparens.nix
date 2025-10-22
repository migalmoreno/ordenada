{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "smartparens"
  ];
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
      package = mkPackageOption pkgs [
        "emacsPackages"
        "smartparens"
      ] { };
      hooks = mkOption {
        type = types.listOf types.str;
        description = "The list of mode hooks where smartparens-mode should be enabled.";
        default = [ "prog-mode-hook" ];
      };
      strictHooks = mkOption {
        type = types.listOf types.str;
        description = "The list of mode hooks where smartparens-strict-mode should be enabled.";
        default = [ "prog-mode-hook" ];
      };
      showSmartParens = mkEnableOption "Smartparens built-in visualization of matching pairs";
      pareditBindings = mkEnableOption "Paredit bindings for Smartparens";
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-smartparens";
        config = with config.ordenada.features.emacs.smartparens; ''
          (eval-when-compile
            (require 'smartparens))
          (autoload 'smartparens-mode "smartparens-autoloads")
          (autoload 'smartparens-strict-mode "smartparens-autoloads")
          (mapcar (lambda (hook)
                    (add-hook hook 'smartparens-mode))
                  '(${toString hooks}))
          (mapcar (lambda (hook)
                    (add-hook hook 'smartparens-strict-mode))
                  '(${toString strictHooks}))

          (require 'smartparens-config)
          (sp-use-${if pareditBindings then "paredit" else "smartparens"}-bindings)
          (setopt sp-highlight-pair-overlay nil)
          (keymap-set smartparens-mode-map "M-s" nil)
          (keymap-set smartparens-mode-map "M-S" #'sp-forward-slurp-sexp)

          (with-eval-after-load 'paren
            (setopt show-paren-style 'mixed)
            ${
              if showSmartParens then
                ''
                  (show-paren-mode -1)
                  (show-smartparens-global-mode 1)
                ''
              else
                ''
                  (show-paren-mode 1)
                ''
            })
        '';
        elispPackages = [ config.ordenada.features.emacs.smartparens.package ];
      };
    };
}
