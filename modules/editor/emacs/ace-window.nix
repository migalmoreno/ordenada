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
      enable = lib.mkEnableOption "the Emacs ace-window feature";
    };
  };
  config = {
    home-manager = mkHomeConfig config "emacs.ace-window" (user: {
      programs.emacs = mkElispConfig {
        name = "ordenada-ace-window";
        config = ''
          (keymap-global-set "M-o" #'ace-window)
          (with-eval-after-load 'ace-window
            (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
            (setopt aw-background nil)
            (setopt aw-scope 'frame)
            (setopt aw-ignore-current nil)
            (setopt aw-display-mode-overlay nil))
        '';
        elispPackages = with pkgs.emacsPackages; [ ace-window ];
      };
    });
  };
}
