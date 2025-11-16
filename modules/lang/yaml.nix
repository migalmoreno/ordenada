{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = "yaml";
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-yaml";
        config = # elisp
          ''
            (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
            (with-eval-after-load 'yaml-mode
              (keymap-set yaml-mode-map "RET" #'newline-and-indent))
          '';
        elispPackages = with pkgs.emacsPackages; [ yaml-mode ];
      };
    };
}
