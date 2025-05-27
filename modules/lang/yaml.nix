{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options.ordenada.features.yaml = {
    enable = lib.mkEnableOption "the YAML feature";
  };
  config = {
    home-manager = mkHomeConfig config "yaml" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-yaml";
        config = ''
          (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
          (with-eval-after-load 'yaml-mode
            (keymap-set yaml-mode-map "RET" #'newline-and-indent))
        '';
        elispPackages = with pkgs.emacsPackages; [ yaml-mode ];
      };
    });
  };
}
