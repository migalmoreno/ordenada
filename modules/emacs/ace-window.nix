{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.emacs.ace-window = {
      enable = lib.mkEnableOption "Emacs ace-window feature.";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.ace-window" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-ace-window";
        config = ''
          (define-key global-map (kbd "M-o") 'ace-window)
          (with-eval-after-load 'ace-window
            (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
            (setq aw-background nil)
            (setq aw-scope 'frame)
            (setq aw-ignore-current nil)
            (setq aw-display-mode-overlay nil))
        '';
        elispPackages = with pkgs.emacsPackages; [ ace-window ];
      };
    });
  };
}
