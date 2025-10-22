{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "orderless"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
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
    };
}
