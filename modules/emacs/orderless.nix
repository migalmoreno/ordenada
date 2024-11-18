{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.orderless = {
      enable = lib.mkEnableOption "the Emacs orderless feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.orderless" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-orderless";
        config = ''
          (with-eval-after-load 'minibuffer
            (setopt orderless-component-separator
                    #'orderless-escapable-split-on-space)
            (setopt completion-styles '(orderless basic))
            (setopt completion-category-overrides
                    '((project-file (styles . (orderless partial-completion basic)))
                      (file (styles . (orderless partial-completion basic)))))
            (setopt enable-recursive-minibuffers t))
        '';
        elispPackages = with pkgs.emacsPackages; [ orderless ];
      };
    });
  };
}
