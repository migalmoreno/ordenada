{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.smartparens = {
      enable = lib.mkEnableOption "the Emacs smartparens feature";
      package = lib.mkPackageOption pkgs [
        "emacsPackages"
        "smartparens"
      ] { };
      hooks = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of mode hooks where smartparens-mode should be enabled.";
        default = [ "prog-mode-hook" ];
      };
      strictHooks = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of mode hooks where smartparens-strict-mode should be enabled.";
        default = [ "prog-mode-hook" ];
      };
      showSmartParens = lib.mkEnableOption "Smartparens built-in visualization of matching pairs";
      pareditBindings = lib.mkEnableOption "Paredit bindings for Smartparens";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.smartparens" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-smartparens";
        config = with user.features.emacs.smartparens; ''
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
        elispPackages = [ user.features.emacs.smartparens.package ];
      };
    });
  };
}
