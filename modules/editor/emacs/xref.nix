{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "xref"
  ];
  homeManager =
    { config, pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-xref";
        config = # elisp
          ''
            (with-eval-after-load 'xref
              (setopt xref-auto-jump-to-first-definition 'move)
              (setopt xref-auto-jump-to-first-xref 'move)
              (setopt xref-prompt-for-identifier
                      '(not xref-find-definitions-other-window
                            xref-find-definitions-other-frame))
              ${lib.optionalString config.ordenada.features.emacs.consult.enable ''
                (setopt xref-show-xrefs-function #'consult-xref)
                (setopt xref-show-definitions-function #'consult-xref)
              ''})
          '';
        elispPackages = with pkgs.emacsPackages; [ rainbow-delimiters ];
      };
    };
}
