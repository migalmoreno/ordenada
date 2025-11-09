{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "embark"
  ];
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-embark";
        config = # elisp
          ''
            (keymap-global-set "C-." #'embark-act)
            (keymap-global-set "C->" #'embark-become)
            (keymap-set minibuffer-local-map "M-g" #'embark-become)
            (keymap-set help-map "b" #'embark-bindings)

            (with-eval-after-load 'embark
              (setopt embark-indicators '(embark-minimal-indicator))
              (setopt embark-prompter #'embark-keymap-prompter)
              (setq prefix-help-command #'embark-prefix-help-command))

            (with-eval-after-load 'window
              (add-to-list 'display-buffer-alist
                             `(,(rx bos "*Embark Collect "
                                    (or "Live" "Completions") "*")
                               nil
                               (window-parameters (mode-line-format . none)))))
          '';
        elispPackages = [
          (pkgs.emacs.pkgs.overrideScope (
            final: prev: with prev.elpaPackages; {
              embark = embark.overrideAttrs (
                lib.const {
                  packageRequires = [
                    compat
                    org
                  ];
                }
              );
            }
          )).embark
        ];
      };
    };
}
