{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "elisp";
  options.ielmKey = lib.mkOption {
    type = lib.types.str;
    description = "Keybinding to launch IELM.";
    default = "I";
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-elisp";
        config =
          with config.ordenada.features; # elisp
          ''
            ${lib.optionalString emacs.flymake.enable ''
              (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
            ''}

            (with-eval-after-load 'elisp-mode
              (let ((map emacs-lisp-mode-map))
                (keymap-set map "C-x C-e" #'pp-eval-last-sexp)
                (keymap-set map "M-:" #'pp-eval-expression)
                (keymap-set map "C-c C-m" #'pp-macroexpand-last-sexp)
                (keymap-set map "C-c C-b" #'eval-buffer)
                ${lib.optionalString emacs.embark.enable ''
                  (autoload 'embark-pp-eval-defun "embark")
                  (keymap-set map "C-c C-c" #'embark-pp-eval-defun)
                ''}))
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${elisp.ielmKey}" #'ielm))

            (with-eval-after-load 'ielm
              (setq ielm-header "")
              (setq ielm-noisy nil))

            ${lib.optionalString emacs.org.enable ''
              (with-eval-after-load 'org
                (add-to-list 'org-structure-template-alist
                             '("el" . "src elisp")))

              (with-eval-after-load 'ob-emacs-lisp
                (setq org-babel-default-header-args:elisp
                      '((:lexical . "t")
                        (:results . "scalar"))))
            ''}
          '';
      };
    };
}
