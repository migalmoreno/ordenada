{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  inherit (lib)
    mkIf
    ;
in
mkFeature {
  name = "yaml";
  homeManager =
    { config, pkgs, ... }:
    {
      ordenada.features.emacs.corfu.globalModes = mkIf (config.ordenada.features.emacs.corfu.enable) [
        "yaml-mode"
      ];

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
